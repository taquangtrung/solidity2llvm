/*
	This file is not a part of Solidity.
*/
/**
 * @author Ta Quang Trung.
 * @date 2018
 * Debugging
 */


#include <iostream>
#include <typeinfo>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/LlvmCompiler.h"


using namespace std;
using namespace dev;
using namespace dev::solidity;


string LlvmCompiler::llvmString(ContractDefinition const* contract,
																StringMap sourceCodes) {
	// llvm::IRBuilder<> Builder1(Context);
	LoopStack.empty();

	cout << endl << "==========================" << endl ;
	cout << "** Compiling Solidity to LLVM IR ..." << endl;

	compilingSourceCodes = sourceCodes;
	compileContract(contract);

	// validate module
	llvm::verifyModule(*CurrentModule);

	cout << endl << "==========================" << endl ;
	cout << "** Output LLVM IR: " << endl << endl;
	CurrentModule->print(llvm::outs(), nullptr);

	std::error_code EC;
	llvm::raw_fd_ostream OS("module", EC, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(&(*CurrentModule), OS);
	OS.flush();

	return "";

}

string LlvmCompiler::llvmString(vector<const ContractDefinition *> contracts) {
	LoopStack.empty();

	cout << endl << "==========================" << endl ;
	cout << "** Compiling Solidity to LLVM IR ..." << endl;
	string sourceFile = contracts.back()->sourceUnitName();
	CurrentModule = llvm::make_unique<llvm::Module>(ContractName, Context);
	CurrentModule->setSourceFileName(contracts.back()->sourceUnitName());
	for(auto contract : contracts)
		compileContracts(contract);

	// validate module
	llvm::verifyModule(*CurrentModule);

	cout << endl << "==========================" << endl ;
	cout << "** Output LLVM IR: " << endl << endl;
	CurrentModule->print(llvm::outs(), nullptr);

	std::error_code EC;
	llvm::raw_fd_ostream OS("module", EC, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(&(*CurrentModule), OS);
	OS.flush();


	return "";
}
/********************************************************
 *               Compile Contract
 ********************************************************/

void LlvmCompiler::compileContract(ContractDefinition const* contract) {
	// prepare environment
	MapGlobalVars.clear();
	MapTupleType.clear();

	// make contract
	ContractName = contract->name();
	CurrentModule = llvm::make_unique<llvm::Module>(ContractName, Context);
	CurrentModule->setSourceFileName(contract->sourceUnitName());

	// structs and enums
	for (StructDefinition const* d: contract->definedStructs())
		compileStructDecl(d);
	for (EnumDefinition const* d: contract->definedEnums())
		compileEnumDecl(d);
	//contract
	compileGlobal(contract->stateVariables());
	//for ()

	// perform optimization passes
	FunctionPM =
		llvm::make_unique<llvm::legacy::FunctionPassManager>(CurrentModule.get());

	// Promote allocas to registers.
	// FunctionPM->add(llvm::createPromoteMemoryToRegisterPass());
	// // Do simple "peephole" optimizations and bit-twiddling optzns.
	// FunctionPM->add(llvm::createInstructionCombiningPass());
 	// Reassociate expressions.
	// FunctionPM->add(llvm::createReassociatePass());
	// Eliminate Common SubExpressions.
	// FunctionPM->add(llvm::createGVNPass());
	// // Simplify the control flow graph (deleting unreachable blocks, etc).
	// FunctionPM->add(llvm::createCFGSimplificationPass());

	FunctionPM->doInitialization();

	for (VariableDeclaration const* var: contract->stateVariables())
		compileGlobalVarDecl(var);

	// functions
	for (FunctionDefinition const* func: contract->definedFunctions())
		compileFuncDecl(func);
}
void LlvmCompiler::compileContracts(ContractDefinition const* contract) {
	// prepare environment
	//MapGlobalVars.clear();
	//MapTupleType.clear();

	// make contract
	ContractName = contract->name();
//	CurrentModule = llvm::make_unique<llvm::Module>(ContractName, Context);
//	CurrentModule->setSourceFileName(contract->sourceUnitName());

	// structs and enums
	for (StructDefinition const* d: contract->definedStructs())
		compileStructDecl(d);
	for (EnumDefinition const* d: contract->definedEnums())
		compileEnumDecl(d);
	//contract
	compileGlobal(contract->stateVariables());
	//for ()

	// perform optimization passes
	FunctionPM =
			llvm::make_unique<llvm::legacy::FunctionPassManager>(CurrentModule.get());

	FunctionPM->doInitialization();

	for (VariableDeclaration const* var: contract->stateVariables())
		compileGlobalVarDecl(var);

	// functions
	for (FunctionDefinition const* func: contract->definedFunctions())
		compileFuncDecl(func);
}

/********************************************************
 *                Compile Declarations
 ********************************************************/

LLStructType* LlvmCompiler::compileStructDecl(StructDefinition const* d) {
	string name = ContractName + "." + d->name();

	// fields of structs
	vector<LLType*> llElems;
	for (auto var : d->members())
		llElems.push_back(compileType(var->type()));

	LLStructType* llType = LLStructType::create(Context, llElems, name, true);
	MapStructTypes[name] = llType;

	return llType;
}

LLIntegerType* LlvmCompiler::compileEnumDecl(EnumDefinition const* d) {
	string llEnumName = d->sourceUnitName() + "." + d->name();

	// map value members of an enum type to integers
	map<string, int> llMemberValues;
	int llMemberValue = 0;
	for (auto m : d->members()) {
		llMemberValues[m->name()] = llMemberValue;
		llMemberValue++;
	}
	MapEnumTypes[llEnumName] = llMemberValues;

	// compile an enum type to an integer type
	return LLIntegerType::get(Context, 64);
}


LLValue* LlvmCompiler::compileGlobalVarDecl(VariableDeclaration const* var) {
	LLType* llType = compileType(var->type());
	string name = var->name();
	LLConstant *llInitValue = nullptr;

	if (Expression* v = var->value().get())
		llInitValue = llvm::dyn_cast<LLConstant>(compileExp(v));

	auto llVar = new LLGlobalVar(*CurrentModule, llType, false,
															 LLGlobalVar::CommonLinkage,
															 llInitValue, name);

	MapGlobalVars[name] = llVar;
	SetGlobalVars.insert(llVar);

	return llVar;
}
LLType * LlvmCompiler::arrayToPointer(ArrayType const * type){
	Type const* baseType = type->baseType();
	if(auto t = dynamic_cast<ArrayType const*>(baseType)){
		LLType * elemeType = arrayToPointer(t);
		return llvm::PointerType::getUnqual(elemeType);
	}
	else{
		return llvm::PointerType::getUnqual(compileType(baseType));
	}

}


LLValue* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var,
																					 Expression const* value) {
	LLType* llType = compileType(var.type());
	string name = var.name();
	LLValue* llVar;
	if (auto t = dynamic_cast<ArrayType const*>(var.type())){
		//use pointer implenment dynamic array
		LLType * baseType = arrayToPointer(t);
		llVar = Builder.CreateAlloca(baseType, nullptr, name);
	}
	else{
		llVar = Builder.CreateAlloca(llType, nullptr, name);
	}

	MapLocalVars[name] = llVar;
	SetLocalVars.insert(llVar);

	if (value == nullptr)
		return llVar;

	//LLValue* llValue = compileExp(value);
	//return Builder.CreateStore(llValue, llVar);
	return  llVar;
}

LLValue* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var) {
	return compileLocalVarDecl(var, nullptr);
}

LLFunction* LlvmCompiler::compileFuncDecl(FunctionDefinition const* func) {
	// prepare environment
	MapLocalVars.clear();

	// function name
	string funcName = func->name();
	cout<<"function:" + funcName<<endl;
	if(funcName==""){
		funcName = ContractName;
	}
	else{
		funcName = ContractName + "." + func->name();
	}

	// function type
	FunctionTypePointer funcType = func->functionType(true);
	LLType* llType = compileType(funcType);
	LLFuncType* llFuncType = llvm::dyn_cast<LLFuncType>(llType);

	// create function
	LLFunction *llFunc = LLFunction::Create(llFuncType,
											LLFunction::CommonLinkage,
											funcName, CurrentModule.get());

	// set names for parameters and also record it to local names
	int index = 0;
	for (auto &arg : llFunc->args()) {
		if(index == 0){
			string paramName = "this";
			arg.setName(paramName);
			MapLocalVars[paramName] = &arg;
		}
		else{
			string paramName = func->parameters().at(index-1)->name();
			arg.setName(paramName);
			MapLocalVars[paramName] = &arg;
		}
		index++;
	}

	// translate new function
	LLBlock *llBlock = LLBlock::Create(Context, ".entry", llFunc);
	Builder.SetInsertPoint(llBlock);
	

	for (auto stmt: func->body().statements())
		compileStmt(*stmt);

	// validate function
	llvm::verifyFunction(*llFunc);

	// run optimization passes
	FunctionPM->run(*llFunc);

	return llFunc;
}

LLStructType * LlvmCompiler::compileGlobal(vector<VariableDeclaration const*> vars){
	string name = "contract." + ContractName;

	// fields of structs
	vector<LLType*> llElems;
	int index = 0;
	for (auto var : vars){
		llElems.push_back(compileType(var->type()));
		MapGlobalVarsIndex[var->name()]=index;
		index++;
	}
	LLStructType* llType = LLStructType::create(Context, llElems, name, true);
	MapStructTypes[name] = llType;

	return llType;

}

/********************************************************
 *                 Compile Statements
 ********************************************************/

void LlvmCompiler::compileStmt(Statement const& stmt) {
	if (auto s = dynamic_cast<InlineAssembly const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<Block const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<PlaceholderStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<IfStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<ForStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<WhileStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<Continue const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<Break const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<Return const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<Throw const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<EmitStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<VariableDeclarationStatement const*>(&stmt))
		return compileStmt(s);

	if (auto s = dynamic_cast<ExpressionStatement const*>(&stmt))
		return compileStmt(s);

	LogError("CompileStmt: Unknown Stmt");
}

void LlvmCompiler::compileStmt(InlineAssembly const* stmt) {
	// TODO
	LogError("Compile InlineAssembly: Need to handle");
}

void LlvmCompiler::compileStmt(Block const* stmt) {
	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();

	LLBlock* llBlock = LLBlock::Create(Context, ".block", llFunc);
	Builder.SetInsertPoint(llBlock);

	for (auto s : stmt->statements())
		compileStmt(*s);
}

void LlvmCompiler::compileStmt(PlaceholderStatement const* stmt) {
	// TODO
	LogError("compileStmt: PlaceholderStatement: unhandled");
}

void LlvmCompiler::compileStmt(IfStatement const* stmt) {
	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();

	LLBlock* llBlockThen = LLBlock::Create(Context, ".if.then", llFunc);
	LLBlock* llBlockElse = LLBlock::Create(Context, ".if.else", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, ".if.end", llFunc);

	LLValue* llCond = compileExp(&(stmt->condition()));
	Builder.CreateCondBr(llCond, llBlockThen, llBlockElse);

	Builder.SetInsertPoint(llBlockThen);
	compileStmt(stmt->trueStatement());
	Builder.CreateBr(llBlockEnd);

	auto elseStmt = stmt->falseStatement();
	if (elseStmt != nullptr) {
		Builder.SetInsertPoint(llBlockElse);
		compileStmt(*elseStmt);
		Builder.CreateBr(llBlockEnd);

		Builder.SetInsertPoint(llBlockEnd);
		// llvm::PHINode *phiNode = Builder.CreatePHI(phiType, 2, "if_stmt");
		// phiNode->addIncoming(thenValue, llBlockThen);
		// phiNode->addIncoming(elseValue, llBlockElse);
		// return phiNode;
	}
}

void LlvmCompiler::compileStmt(WhileStatement const* stmt) {
	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();

	LLBlock* llBlockCond = LLBlock::Create(Context, ".while.cond", llFunc);
	LLBlock* llBlockBody = LLBlock::Create(Context, ".while.body", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, ".while.end", llFunc);

	// store to the loop stack
	LoopInfo llLoop {llBlockCond, llBlockEnd};
	LoopStack.push(llLoop);

	// loop condition
	Builder.SetInsertPoint(llBlockCond);
	LLValue* llCond = compileExp(&(stmt->condition()));
	Builder.CreateCondBr(llCond, llBlockBody, llBlockEnd);

	// loop body
	Builder.SetInsertPoint(llBlockBody);
	compileStmt(stmt->body());
	Builder.CreateBr(llBlockCond);

	// end block
	Builder.SetInsertPoint(llBlockEnd);

	// remove the loop from the stack
	LoopStack.pop();
}

void LlvmCompiler::compileStmt(ForStatement const* stmt) {
	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();

	LLBlock* llBlockCond = LLBlock::Create(Context, ".for.cond", llFunc);
	LLBlock* llBlockLoop = LLBlock::Create(Context, ".for.loop", llFunc);
	LLBlock* llBlockBody = LLBlock::Create(Context, ".for.body", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, ".for.end", llFunc);

	// store to the loop stack
	LoopInfo llLoop {llBlockCond, llBlockEnd};
	LoopStack.push(llLoop);

	// loop initialization
	compileStmt(*(stmt->initializationExpression()));
	Builder.CreateBr(llBlockCond);

	// loop condition
	Builder.SetInsertPoint(llBlockCond);
	LLValue* llCond = compileExp(stmt->condition());
	Builder.CreateCondBr(llCond, llBlockBody, llBlockEnd);

	// loop expression
	Builder.SetInsertPoint(llBlockLoop);
	compileStmt(stmt->loopExpression());
	Builder.CreateBr(llBlockCond);

	// loop body
	Builder.SetInsertPoint(llBlockBody);
	compileStmt(stmt->body());
	Builder.CreateBr(llBlockLoop);

	// end block
	Builder.SetInsertPoint(llBlockEnd);

	// remove the loop from the stack
	LoopStack.pop();
}

void LlvmCompiler::compileStmt(Continue const* stmt) {
	LoopInfo llLoop = LoopStack.top();
	Builder.CreateBr(llLoop.loopHead);
}

void LlvmCompiler::compileStmt(Break const* stmt) {
	LoopInfo llLoop = LoopStack.top();
	Builder.CreateBr(llLoop.loopEnd);
}

void LlvmCompiler::compileStmt(Return const* stmt) {
	Expression const* returnExp = stmt->expression();

	if (auto tupleExp = dynamic_cast<TupleExpression const*>(returnExp)) {
		// return a tuple expression
		ReturnAnnotation& annot = stmt->annotation();
		ParameterList const* returnParams = annot.functionReturnParameters;
		string tupleName = "tuple";
		for (ASTPointer<VariableDeclaration> var : returnParams->parameters())
			tupleName = tupleName + "." + var->type()->canonicalName();

		LLType* llRetType = MapTupleType[tupleName];
		LLValue* llReturnExp = Builder.CreateAlloca(llRetType);

		int index = 0;
		for (ASTPointer<Expression> elem : tupleExp->components()) {
			LLValue* llElemValue = compileExp(&(*elem));
			vector<LLValue*> valueIndex = makeIndexGEP({0, index});
			LLValue* llElemAddr = Builder.CreateGEP(llReturnExp, valueIndex);
			Builder.CreateStore(llElemValue, llElemAddr);
			index++;
		}

		Builder.CreateRet(llReturnExp);
	}
	else {
		// return a normal expression
		//when the value
		LLValue * returnValue = compileExp(returnExp);
		if(llvm::dyn_cast<LLPointerType>(returnValue->getType())){
			returnValue = Builder.CreateLoad(returnValue);
		}
		Builder.CreateRet(returnValue);
	}
}

void LlvmCompiler::compileStmt(Throw const* stmt) {
	LogError("Compiling Throw Stmt");
}

void LlvmCompiler::compileStmt(EmitStatement const* stmt) {
	compileExp(&(stmt->eventCall()));
}

void LlvmCompiler::compileStmt(VariableDeclarationStatement const* stmt) {
	Expression const* initValue = stmt->initialValue();

	// no initialization
	if (initValue == nullptr) {
		for (ASTPointer<VariableDeclaration> var : stmt->declarations())
			compileLocalVarDecl(*var, nullptr);
		return;
	}

	LLValue* llInitValue = compileExp(initValue);
	LogDebug("initValue",initValue);
	// initialized by a tuple
	if (auto tplValue = dynamic_cast<TupleExpression const*>(initValue)) {
		int index = 0;
		vector<ASTPointer<Expression>> elems = tplValue->components();
		for (ASTPointer<VariableDeclaration> var : stmt->declarations()) {
			if (var != nullptr) {
				Expression &elem = *(elems.at(index));
				compileLocalVarDecl(*var, &elem);
			}
			index++;
		}
		return;
	}

	// initialized by a function call
	// extend for new expression or new contract
	if (auto t = dynamic_cast<FunctionCall const*>(initValue)) {

		int index = 0;
		// fixes bug for only one return of function eg. uint a = f();
		if(stmt->declarations().size()>1) {
			//function return is a tuple,need base alloca and store
			LLType* llBaseType = compileType(initValue->annotation().type);
			LLValue* llBaseValue = Builder.CreateAlloca(llBaseType);
			Builder.CreateStore(llInitValue, llBaseValue);
			// function return is a tuple
			for (ASTPointer<VariableDeclaration> var : stmt->declarations()) {
				if (var != nullptr) {
					LLValue *llVar = compileLocalVarDecl(*var);
					vector<LLValue *> gepIndices = makeIndexGEP({0, index});
					LLValue *llVarVal = Builder.CreateGEP(llBaseValue, gepIndices);
					Builder.CreateStore(llVarVal, llVar);
				}
				index++;
			}
	}
		else{
			// only one return
			LLValue* llVar = compileLocalVarDecl(*(stmt->declarations().at(0)),t);
			LogDebug("one_return_llInitValue",llInitValue);
			LogDebug("one_return_llVar",llVar);
			Builder.CreateStore(llInitValue, llVar);
		}
		return;
	}

	// initialized by other expressions
	for (ASTPointer<VariableDeclaration> var : stmt->declarations())
		if (var != nullptr) {
			LLValue* llVar = compileLocalVarDecl(*var);
			Builder.CreateStore(llInitValue, llVar);
		}

}

void LlvmCompiler::compileStmt(ExpressionStatement const* stmt) {
	compileExp(&(stmt->expression()));
}

/********************************************************
 *                Compile Expressions
 ********************************************************/

LLValue* LlvmCompiler::compileExp(Expression const* exp) {
	if (auto e = dynamic_cast<Conditional const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<Assignment const*>(exp))
	  return compileExp(e);

	if (auto e = dynamic_cast<TupleExpression const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<UnaryOperation const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<BinaryOperation const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<FunctionCall const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<NewExpression const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<MemberAccess const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<IndexAccess const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<PrimaryExpression const*>(exp))
		return compileExp(e);

	LogError("compileExp: unknown expression: ", exp);
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Conditional const* exp) {
	// TODO Kunpeng
	/**
	 * tranfer conditional to if-else statement
	 */
	Expression const& condExp = exp->condition();
	Expression const& trueExp = exp->trueExpression();
	Expression const& falseExp = exp->falseExpression();

	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();
	LLBlock* llBlockThen = LLBlock::Create(Context, ".if.then", llFunc);
	LLBlock* llBlockElse = LLBlock::Create(Context, ".if.else", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, ".if.end", llFunc);

	LLValue* llCond = compileExp(&condExp);
	Builder.CreateCondBr(llCond, llBlockThen, llBlockElse);
	Builder.SetInsertPoint(llBlockThen);
	compileExp(&trueExp);
	Builder.CreateBr(llBlockEnd);

	Builder.SetInsertPoint(llBlockElse);
	compileExp(&falseExp);
	return Builder.CreateBr(llBlockEnd);
}

LLValue* LlvmCompiler::compileExp(Assignment const* exp) {
	Expression const& lhs = exp->leftHandSide();
	Expression const& rhs = exp->rightHandSide();

	// LHS is a tuple
	if (auto tupleLhs = dynamic_cast<TupleExpression const*>(&lhs)) {
		// RHS is a tuple
		if (auto tupleRhs = dynamic_cast<TupleExpression const*>(&rhs)) {
			vector<LLValue*> llRhsElems;
			for (ASTPointer<Expression> elemRhs : tupleRhs->components()) {
				llRhsElems.push_back(compileRhsExp(&(*elemRhs)));
			}
			int index = 0;
			LLValue* llAssign;
			for (ASTPointer<Expression> elemLhs : tupleLhs->components()) {
				if (elemLhs != nullptr) {
					LLValue* llElemRhs = llRhsElems.at(index);
					LLValue* llElemLhs = compileExp(&(*elemLhs));
					llAssign = Builder.CreateStore(llElemRhs, llElemLhs);
				}
				index++;
			}
			return llAssign;
		}

		// RHS is a function call
		if (dynamic_cast<FunctionCall const*>(&rhs)) {
			LLValue* llRhs = compileRhsExp(&rhs);
			LLType* llRhsBaseType = compileType(rhs.annotation().type);
			LLValue* llRhsBaseValue = Builder.CreateAlloca(llRhsBaseType);
			Builder.CreateStore(llRhs, llRhsBaseValue);
			int index = 0;
			LLValue* llAssign;
			for (ASTPointer<Expression> elemLhs : tupleLhs->components()) {
				if (elemLhs != nullptr) {
					LLValue* llElemLhs = compileExp(&(*elemLhs));
					vector<LLValue*> gepIndices = makeIndexGEP({0, index});
					LLValue* llElemRhs = Builder.CreateGEP(llRhsBaseValue, gepIndices);
					llAssign = Builder.CreateStore(llElemRhs, llElemLhs);
				}
				index++;
			}
			return llAssign;
		}

		// unknown RHS expression
		LogError("Compile Assignment: unknown RHS expression");
		return nullptr;
	}
	// RH is conditional expression
	// eg:   d = a > b ? a : b;
	if (auto rhs_cond = dynamic_cast<Conditional const*>(&rhs)){
		LLValue* llLhs = compileExp(&lhs);
		Expression const& condExp = rhs_cond->condition();
		Expression const& trueExp = rhs_cond->trueExpression();
		Expression const& falseExp = rhs_cond->falseExpression();
		//select
		//return  Builder.CreateSelect(llCond,compileExp(&trueExp),compileExp(&falseExp));
		LLFunction* llFunc = Builder.GetInsertBlock()->getParent();
		LLValue* llCond = compileExp(&condExp);
		LLBlock* llBlockThen = LLBlock::Create(Context, ".if.then", llFunc);
		LLBlock* llBlockElse = LLBlock::Create(Context, ".if.else", llFunc);
		LLBlock* llBlockEnd = LLBlock::Create(Context, ".if.end", llFunc);

		Builder.CreateCondBr(llCond, llBlockThen, llBlockElse);

		Builder.SetInsertPoint(llBlockThen);
		LLValue* llRhsTrue = compileRhsExp(&trueExp);
		Builder.CreateStore(llRhsTrue, llLhs);
		Builder.CreateBr(llBlockEnd);
		Builder.SetInsertPoint(llBlockElse);
		LLValue* llRhsFalse = compileRhsExp(&falseExp);
		Builder.CreateStore(llRhsFalse, llLhs);
		return Builder.CreateBr(llBlockEnd);
	}
	// other cases

	LLValue* llRhs = compileRhsExp(&rhs);
	LogDebug("rhs",llRhs);
	LLValue* llLhs = compileExp(&lhs);
	LogDebug("lhs",llLhs);
	// store first argument is value, seconde value is pointer of first type

	return Builder.CreateStore(llRhs, llLhs);
}

LLValue* LlvmCompiler::compileExp(TupleExpression const* exp) {
	// tuples are handled directly at assignment, function call, or return
	LogError("Compile TupleExpression: not expecting tuple!\n"
					 "(Tuples are handled directly at assignment,"
					 " function call, or return.)");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(UnaryOperation const* exp) {
	LLValue* llSubExp = compileExp(&(exp->subExpression()));
	if (!llSubExp) return nullptr;

	switch (exp->getOperator()) {
	case Token::Not:
		return Builder.CreateNot(llSubExp);

	case Token::BitNot:
		return Builder.CreateNot(llSubExp);

	case Token::Inc: {
		LLValue* llOne = LLConstantInt::get(llSubExp->getType(), 1);
		LLValue* llSubExpValue = llSubExp;
		if(llvm::dyn_cast<LLPointerType >(llSubExp->getType())){
			llSubExpValue = Builder.CreateLoad(llSubExp);
		}
		LLValue* llResult = Builder.CreateAdd(llSubExpValue, llOne);
		return Builder.CreateStore(llResult, llSubExp);
	}

	case Token::Dec: {
		LLValue* llOne = LLConstantInt::get(llSubExp->getType(), 1);
		LLValue* llSubExpValue = llSubExp;
		if(llvm::dyn_cast<LLPointerType >(llSubExp->getType())){
			llSubExpValue = Builder.CreateLoad(llSubExp);
		}
		LLValue* llResult = Builder.CreateSub(llSubExpValue, llOne);
		return Builder.CreateStore(llResult, llSubExp);
	}

	case Token::Delete:
		LogError("compileExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("compileExp: UnaryOp: unknown operator: ", exp);
		return nullptr;
	}
}

LLValue* LlvmCompiler::compileExp(BinaryOperation const* exp) {
	Expression const& lhs = exp->leftExpression();
	Expression const& rhs = exp->rightExpression();
	Type const* lhsType = lhs.annotation().type;
	Type const* rhsType = rhs.annotation().type;


	bool isIntegerExp = false;
	bool isSignedIntegerExp = false;
	int numBitLhs = 0;
	int numBitRhs = 0;
	if (auto t = dynamic_cast<IntegerType const*>(lhsType)) {
		isIntegerExp = true;
		if (t->isSigned())
			isSignedIntegerExp = true;
		numBitLhs = t->numBits();
	}
	if (auto t = dynamic_cast<IntegerType const*>(rhsType)) {
		isIntegerExp = true;
		if (t->isSigned())
			isSignedIntegerExp = true;
		numBitRhs = t->numBits();
	}

	LLValue* llLhs = compileExp(&lhs);
	LLValue* llRhs = compileExp(&rhs);


	// casting types for integer expressions
	if (isIntegerExp && (numBitLhs != numBitRhs)) {
		if (isSignedIntegerExp) {
			if (numBitLhs > numBitRhs)
				llRhs = Builder.CreateSExt(llRhs, llLhs->getType());
			else
				llLhs = Builder.CreateSExt(llLhs, llRhs->getType());
		}
		else {
			if (numBitLhs > numBitRhs)
				llRhs = Builder.CreateZExt(llRhs, llLhs->getType());
			else
				llLhs = Builder.CreateZExt(llLhs, llRhs->getType());
		}
	}

	if (!llLhs || !llRhs) return nullptr;
	// load the value of pointer type
	if(llvm::dyn_cast<LLPointerType >(llLhs->getType())){
		llLhs = Builder.CreateLoad(llLhs);
	}
	if(llvm::dyn_cast<LLPointerType >(llRhs->getType())){
		llRhs = Builder.CreateLoad(llRhs);
	}
	switch (exp->getOperator()) {
	case Token::Equal:
		if (isIntegerExp)
			return Builder.CreateICmpEQ(llLhs, llRhs);
		else {
			LogError("Compile BinaryOperation: Unknown Equal Exp");
			return nullptr;
		}

	case Token::NotEqual:
		if (isIntegerExp)
			return Builder.CreateICmpNE(llLhs, llRhs);
		else {
			LogError("Compile BinaryOperation: Unknown Equal Exp");
			return nullptr;
		}

	case Token::LessThan:
		if (isIntegerExp) {
			if (isSignedIntegerExp)
				return Builder.CreateICmpSLT(llLhs, llRhs);
			else
				return Builder.CreateICmpULT(llLhs, llRhs);
		}
		else {
			LogError("Compile BinaryOperation: Unknown LessThan Exp");
			return nullptr;
		}

	case Token::GreaterThan:
		if (isIntegerExp) {
			if (isSignedIntegerExp)
				return Builder.CreateICmpSGT(llLhs, llRhs);
			else
				return Builder.CreateICmpUGT(llLhs, llRhs);
		}
		else {
			LogError("Compile BinaryOperation: Unknown GreaterThan Exp");
			return nullptr;
		}

	case Token::LessThanOrEqual:
		if (isIntegerExp) {
			if (isSignedIntegerExp)
				return Builder.CreateICmpSLE(llLhs, llRhs);
			else
				return Builder.CreateICmpULE(llLhs, llRhs);
		}
		else {
			LogError("Compile BinaryOperation: Unknown LessThanOrEqual Exp");
			return nullptr;
		}

	case Token::GreaterThanOrEqual:
		if (isIntegerExp) {
			if (isSignedIntegerExp)
				return Builder.CreateICmpSGE(llLhs, llRhs);
			else
				return Builder.CreateICmpUGE(llLhs, llRhs);
		}
		else {
			LogError("Compile BinaryOperation: Unknown GreaterThanOrEqual Exp");
			return nullptr;
		}

	case Token::Comma:
		LogError("compileExp: BinaryOp: need to support Comma Exp");
		return nullptr;

	case Token::Or:
		return Builder.CreateOr(llLhs, llRhs);

	case Token::And:
		return Builder.CreateOr(llLhs, llRhs);

	case Token::BitOr:
		return Builder.CreateOr(llLhs, llRhs);

	case Token::BitXor:
		return Builder.CreateXor(llLhs, llRhs);

	case Token::BitAnd:
		return Builder.CreateXor(llLhs, llRhs);

	case Token::SHL:
		return Builder.CreateShl(llLhs, llRhs);

	case Token::SAR:
		LogError("compileExp: BinaryOp: need to support SAR");
		return nullptr;

	case Token::SHR:
		return Builder.CreateLShr(llLhs, llRhs);

	case Token::Add:
		return Builder.CreateAdd(llLhs, llRhs);

	case Token::Sub:
		return Builder.CreateSub(llLhs, llRhs);

	case Token::Mul:
		return Builder.CreateMul(llLhs, llRhs);

	case Token::Div:
		return Builder.CreateUDiv(llLhs, llRhs);

	case Token::Mod:
		return Builder.CreateURem(llLhs, llRhs);

	case Token::Exp:
		LogError("compileExp: BinaryOp: unhandled Exponential Exp");
		return nullptr;

	default:
		LogError("compileExp: BinaryOp: unknown operator: ", exp);
		return nullptr;
	}
}


LLValue* LlvmCompiler::compileExp(FunctionCall const* exp) {

	FunctionCallAnnotation &annon = exp->annotation();
	LLType* llType = compileType(annon.type);

	// a call to a normal function
	if (annon.kind == FunctionCallKind::FunctionCall) {
		// new expression

		Expression const* m_expression = &(exp->expression());
		// new a dynamic array
		if(auto newExp = dynamic_cast<NewExpression const*>(m_expression)){
			LogDebug("new expression", m_expression);
			if(auto  arrayType = dynamic_cast<ArrayType const*>(annon.type)){
				LLType * baseType;
				if(auto t = dynamic_cast<ArrayType const*>(arrayType->baseType())){
					baseType = arrayToPointer(t);
				}
				else{
					baseType = compileType(arrayType->baseType());
				}
				vector<LLValue*> llArgs;
				for (auto arg : exp->arguments()) {
					LLValue * argValue = compileExp((&arg)->get());
					if(llvm::dyn_cast<LLPointerType >(argValue->getType())){
						argValue = Builder.CreateLoad(argValue);
					}
					llArgs.push_back(argValue);
				}
				LLValue * array =  Builder.CreateAlloca(baseType, llArgs[0],"");
				return  array;
			}
			// new a contract
			else if(auto t = dynamic_cast<ContractType const*>(annon.type)){
				cout<< "newexpression_contract"<<endl;
				LLType * contractType = compileType(t);
				LogDebug("contractType",contractType);
				LLValue * contract = Builder.CreateAlloca(contractType);
				int index = 0;
				for (auto arg : exp->arguments()){
					//llArgs.push_back(compileType(((&arg)->get())->annotation().type));

					LLValue* argValue = compileExp((&arg)->get());
					vector<LLValue*> gepIndices = makeIndexGEP({0,index});
					LogDebug("arg",argValue);
					LLValue* llValue = Builder.CreateGEP(contract, gepIndices);
					Builder.CreateStore(argValue, llValue);
					index++;
				}
				//Builder.Store
				return contract;
			}
		}


		FunctionTypePointer funcType = dynamic_cast<FunctionType const*>(exp->expression().annotation().type);
		FunctionType const& functionType = *funcType;

		//LogDebug("FuncType:", funcType);
		switch (functionType.kind()) {
		case FunctionType::Kind::Assert:
		case FunctionType::Kind::Require:
			LogError("Compile FunctionCall: handle Assert/Require");
			return nullptr;

		case FunctionType::Kind::Internal: {
			string funcName = getFunctionName(exp);
			cout<< "funcName:"<<funcName<<endl;
			LLFunction *llFunc = CurrentModule->getFunction(funcName);

			LogDebug("llFunc:", llFunc);

			vector<LLValue*> llArgs;
			for (auto arg : exp->arguments())
				llArgs.push_back(compileExp((&arg)->get()));

			return Builder.CreateCall(llFunc, llArgs);
		}
		//external contract function call e.g a.set(x); a is contract object
		case FunctionType::Kind::External: {
			Expression const& baseExp = exp->expression();
			string funcName = getFunctionName(exp);
			LLFunction * llFunc = CurrentModule->getFunction(funcName);
			vector<LLValue*> llArgs;
			if(auto exp = dynamic_cast<MemberAccess const*> (&baseExp))
				llArgs.push_back(compileExp(&(exp->expression())));
			for (auto arg : exp->arguments())
				llArgs.push_back(compileExp((&arg)->get()));
			return Builder.CreateCall(llFunc,llArgs);
		}
		default:
			LogError("Compile FunctionCall: Unknown FunctionCall");
			return nullptr;
		}

		LogError("Compile FunctionCall: Unknown FunctionCall");
		return nullptr;
	}

	// a call to a constructor of a struct
	if (annon.kind == FunctionCallKind::StructConstructorCall) {
		vector<LLConstant*> llArgs;
		for (auto arg : exp->arguments())
			if (auto a = llvm::dyn_cast<LLConstant>(compileExp((&arg)->get())))
				llArgs.push_back(a);

		if (auto llStructType = llvm::dyn_cast<LLStructType>(llType))
			return llvm::ConstantStruct::get(llStructType, llArgs);
	}

	// a call to a type conversion
	if (annon.kind == FunctionCallKind::TypeConversion) {
		LLValue* llArg = compileRhsExp((&(exp->arguments().at(0)))->get());

		if (auto t = dynamic_cast<IntegerType const*>(annon.type)) {
			if (t->isSigned())
				return Builder.CreateSExtOrTrunc(llArg, llType);
			else
				return Builder.CreateZExtOrTrunc(llArg, llType);
		}
		else
			return Builder.CreateZExtOrTrunc(llArg, llType);
	}

	// invalid function call
  LogError("CompileFunctionCall: Unknown FunctionCall Type");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(NewExpression const* exp) {

	return nullptr;
}

LLValue* LlvmCompiler::compileExp(MemberAccess const* exp) {
	TypePointer memType = exp->annotation().type;
	LLType* llMemType = compileType(memType);

	Expression const& baseExp = exp->expression();
	TypePointer baseType = baseExp.annotation().type;

	LLValue* llBaseExp = compileExp(&baseExp);

	LLType* llBaseType = compileType(baseType);

	if (dynamic_cast<TypeType const*>(baseType)) {
		if (dynamic_cast<EnumType const*>(memType)) {
			string llEnumName = memType->canonicalName();
			map<string, int> llMemberValues = MapEnumTypes[llEnumName];
			int value = llMemberValues[exp->memberName()];
			LLIntegerType* llIntType = LLIntegerType::get(Context, 64);
			return LLConstantInt::get(llIntType, value);
		}
	}

	if (auto structType = dynamic_cast<StructType const*>(baseType)) {
		string llStructName = baseType->canonicalName();
		LLStructType* llStructType = MapStructTypes[llStructName];
		StructDefinition const& structDef = structType->structDefinition();

		string memberName = exp->memberName();
		int index = 0;
		for (auto var : structDef.members()) {
			if (var->name() == memberName) {
				llMemType = compileType(var->type());
				break;
			}
			index++;
		}

		vector<LLValue*> valueIndex = makeIndexGEP({0, index});
		LogDebug("structbase",llBaseExp);
		return Builder.CreateGEP(llBaseExp, valueIndex);
	}

	LogError("compileExp: MemberAccess: unknown exp: ", exp);

	return nullptr;
}

LLValue* LlvmCompiler::compileExp(IndexAccess const* exp) {
	// TODO Kunpeng
	Expression const* baseExp = &exp->baseExpression();
	Expression const* indexExp = exp->indexExpression();
	LLValue* llBaseExp;
	LLValue* llIndexExp = compileExp(indexExp);;
	//memory array
	if(auto baseType = dynamic_cast<ArrayType const*>(baseExp->annotation().type) )
	{	//memory array need load pointer
		if(baseType->isDynamicallySized()){
			LLValue * llexp = compileExp(baseExp);
			llBaseExp = Builder.CreateLoad(llexp);
			vector<LLValue*> valueIndex;
			valueIndex.push_back(llIndexExp);
			return Builder.CreateGEP(llBaseExp, valueIndex);
		}
		else{
			llBaseExp = compileExp(baseExp);
			vector<LLValue*> valueIndex;
			valueIndex.push_back(LLConstantInt::get(LLType::getInt32Ty(Context), 0));
			valueIndex.push_back(llIndexExp);
			return Builder.CreateGEP(llBaseExp, valueIndex);
		}
	}
}

LLValue* LlvmCompiler::compileExp(PrimaryExpression const* exp) {
	if (auto e = dynamic_cast<Identifier const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<ElementaryTypeNameExpression const*>(exp))
		return compileExp(e);

	if (auto e = dynamic_cast<Literal const*>(exp))
		return compileExp(e);

	LogError("Compile PrimaryExpression: Unknown Expression");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Identifier const* exp) {
	return findNamedValue(exp->name());
}

LLValue* LlvmCompiler::compileExp(ElementaryTypeNameExpression const* exp) {
	LogError("compileExp: ElementaryTypeNameExpression: unhandled");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Literal const* exp) {
	string content = exp->value();
	switch (exp->token()) {

	case Token::StringLiteral:
		return llvm::ConstantDataArray::getString(Context, content);

	case Token::TrueLiteral:
		return LLConstantInt::getTrue(Context);

	case Token::FalseLiteral:
		return LLConstantInt::getFalse(Context);

	case Token::Number: {
		// FIXME: temporarily fix to 64 bit, signed integer
		LLIntegerType* intType = LLIntegerType::get(Context, 64);
		return LLConstantInt::get(intType, atoi(content.data()), true);
	}

	case Token::CommentLiteral:
		LogError("compileExp: CommentLiteral: unhandled");
		return nullptr;

	default:
		LogError("compileExp: Literal: unknown token: ", exp);
		return nullptr;
	}
}


/********************************************************
 * Compile special expressions
 ********************************************************/

vector<LLValue*> LlvmCompiler::compileTupleExp(TupleExpression const* exp) {
	vector<LLValue*> llElems;

	for (ASTPointer<Expression> elem : exp->components()) {
		LLValue* llElem = compileExp(&(*elem));
		llElems.push_back(llElem);
	}

	return llElems;
}


/********************************************************
 *                Auxiliary LLVM functions
 ********************************************************/

LLValue* LlvmCompiler::compileRhsExp(Expression const* exp) {
	LLValue* llArg = compileExp(exp);

	// lookup local vars first
	if (SetLocalVars.find(llArg) != SetLocalVars.end())
		return Builder.CreateLoad(llArg);

	// then global vars
	if (SetGlobalVars.find(llArg) != SetGlobalVars.end())
		return Builder.CreateLoad(llArg);

	// otherwise, the current value is intermediate
	return llArg;
}

/********************************************************
 *                     Compile types
 ********************************************************/

LLType* LlvmCompiler::compileTypeName(TypeName const* type) {
	if (auto t = dynamic_cast<ElementaryTypeName const*>(type))
		return compileTypeName(t);

	if (auto t = dynamic_cast<UserDefinedTypeName const*>(type))
		return compileTypeName(t);

	if (auto t = dynamic_cast<FunctionTypeName const*>(type))
		return compileTypeName(t);

	if (auto t = dynamic_cast<Mapping const*>(type))
		return compileTypeName(t);

	if (auto t = dynamic_cast<ArrayTypeName const*>(type))
		return compileTypeName(t);

	LogError("Compiletypename: Unknown TypeName");
	return nullptr;
}

LLType* LlvmCompiler::compileTypeName(ElementaryTypeName const* type) {
	TypePointer ty = TypeProvider::fromElementaryTypeName(type->typeName());
	return compileType(ty);
}

LLType* LlvmCompiler::compileTypeName(UserDefinedTypeName const* type) {
	// TODO
	LogError("compileTypeName: UserDefinedTypeName: unhandled");
	return nullptr;
}

LLType* LlvmCompiler::compileTypeName(FunctionTypeName const* type) {
	// TODO
	LogError("compileTypeName: FunctionTypeName: unhandled");
	return nullptr;
}

LLType* LlvmCompiler::compileTypeName(Mapping const* type) {
	// TODO Kunpeng
	ElementaryTypeName const &keyType = type->keyType();
	TypeName const &valueType = type->valueType();
	compileTypeName(&keyType);
	compileTypeName(&valueType);

	return nullptr;
}

LLType* LlvmCompiler::compileTypeName(ArrayTypeName const* type) {
	// TODO
	uint64_t size = (uint64_t)type->length();
	LLType* llBaseType = compileTypeName(&type->baseType());
	return LLArrayType::get(llBaseType, size);
}

LLType* LlvmCompiler::compileType(TypePointer type) {
	if (type == nullptr) {
		LogError("compileType: input is null type");
		return nullptr;
	}

	if (auto t = dynamic_cast<IntegerType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<FixedPointType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<RationalNumberType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<StringLiteralType const*>(type))
		return compileType(t);

	if (auto t =  dynamic_cast<BoolType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<StructType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<FixedBytesType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<ArrayType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<ContractType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<EnumType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<TupleType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<FunctionType const*>(type))
		return compileType(t);

	if (auto t= dynamic_cast<MappingType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<TypeType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<ModifierType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<ModuleType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<MagicType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<InaccessibleDynamicType const*>(type))
		return compileType(t);

	if (auto t = dynamic_cast<AddressType const*>(type))
		return compileType(t);

	LogError("CompileType: Unknown Type");
	return nullptr;
}

LLType* LlvmCompiler::compileType(IntegerType const* type) {
	return LLIntegerType::get(Context, type->numBits());
}

LLType* LlvmCompiler::compileType(FixedPointType const* type) {
	// TODO
	LogError("FixedPointType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(RationalNumberType const* type) {
	if (type->isFractional()) {
		LogError("RationalNumberType: handle fractional");
		return nullptr;
	}
	else {
		IntegerType const* intType = type->integerType();
		return LLIntegerType::get(Context, intType->numBits());
	}
}

LLType* LlvmCompiler::compileType(StringLiteralType const* type) {
	// TODO
	LogError("StringLiteralType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(BoolType const* type) {
	return LLIntegerType::get(Context, 8);
}

LLType* LlvmCompiler::compileType(StructType const* type) {
	return MapStructTypes[type->canonicalName()];
}

LLType* LlvmCompiler::compileType(FixedBytesType const* type) {
	uint64_t size = type->numBytes();
	LLIntegerType* llBaseType = LLIntegerType::get(Context, 8);
	return LLArrayType::get(llBaseType, size);
}

LLType* LlvmCompiler::compileType(ArrayType const* type) {
	uint64_t size = (uint64_t)type->length();
	LLType* llBaseType = compileType(type->baseType());
	return LLArrayType::get(llBaseType, size);
}

LLType* LlvmCompiler::compileType(ContractType const* type) {
	// TODO
	string name = "contract." + type->canonicalName();
	cout<<"contractname"<<name<<endl;
	return MapStructTypes[name];
}

LLType* LlvmCompiler::compileType(EnumType const* type) {
	// compile an enum type to an integer type
	return LLIntegerType::get(Context, 64);
}

LLType* LlvmCompiler::compileType(TupleType const* type) {
	string tupleName = "tuple";
	for (Type const* t : type->components())
		tupleName = tupleName + "." + t->canonicalName();

	LLType* llTupleType = MapTupleType[tupleName];

	if (llTupleType == nullptr) {
		vector<LLType*> llElemTypes;
		for (Type const* t : type->components())
			llElemTypes.push_back(compileType(t));
		llTupleType = LLStructType::create(Context, llElemTypes, tupleName);
		MapTupleType[tupleName] = llTupleType;
	}

	return llTupleType;
}

LLType* LlvmCompiler::compileType(FunctionType const* type) {
	// return type
	vector<TypePointer> returnTypes = type->returnParameterTypes();
	LLType* llRetType;
	switch (returnTypes.size()) {
	case 0:
		llRetType = LLType::getVoidTy(Context);
		break;
	case 1:
		llRetType = compileType(returnTypes.at(0));
		break;
	default:
		// Create a struct type to capture this returned type
		string tupleName = "tuple";
		for (Type const* type : returnTypes)
			tupleName = tupleName + "." + type->canonicalName();

		llRetType = MapTupleType[tupleName];

		if (llRetType == nullptr) {
			vector<LLType*> llReturnElemTypes;
			for (Type const* type : returnTypes)
				llReturnElemTypes.push_back(compileType(type));
			llRetType = LLStructType::create(Context, llReturnElemTypes,
																			 tupleName, true);
			MapTupleType[tupleName] = llRetType;
		}
	}

	// params type
	vector<LLType*> llParamTypes;
	LLStructType * contract = MapStructTypes["contract." + ContractName];
	llParamTypes.push_back(llvm::PointerType::getUnqual(contract));
	for (Type const* t: type->parameterTypes())
		llParamTypes.push_back(compileType(t));

	return LLFuncType::get(llRetType, llParamTypes, false);
}

LLType* LlvmCompiler::compileType(MappingType const* type) {
	// TODO Kunpeng
	TypePointer keyType = type->keyType();
	TypePointer valueType = type->valueType();
	compileType(keyType);
	compileType(valueType);
	return nullptr;
}

LLType* LlvmCompiler::compileType(TypeType const* type) {
	// TODO
	LogError("TypeType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(ModifierType const* type) {
	// TODO
	LogError("ModifierType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(ModuleType const* type) {
	// TODO
	LogError("ModuleType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(MagicType const* type) {
	// TODO
	LogError("MagicType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(InaccessibleDynamicType const* type) {
	// TODO
	LogError("InaccessibleDynamicType");
	return nullptr;
}

LLType* LlvmCompiler::compileType(AddressType const* type) {
	// Soldity's address is 20 bytes
	auto byteType = LLIntegerType::get(Context, 8);
	return LLArrayType::get(byteType, 20);
}


/********************************************************
 *                Supporting Functions
 ********************************************************/

string LlvmCompiler::stringOf(llvm::Module* module) {
	string result = "";
	// structs
	for (LLType* structType : module->getIdentifiedStructTypes())
		result = result + "\n**************\n\n" + stringOf(structType);
	return result;
	// functions
	for (auto &func : module->getFunctionList())
		result = result + "\n**************\n\n" + stringOf(&func);
	return result;
}

string LlvmCompiler::stringOf(LLFunction* func) {
	string result = "";
	for (auto &block : func->getBasicBlockList()) {
		result = result + stringOf(&block) + "\n\n";
	}
	return result;
}

string LlvmCompiler::stringOf(LLBlock* block) {
	string result = "";
	for (auto &inst : block->getInstList()) {
		result = result + stringOf(&inst) + "\n";
	}
	return result;
}

string LlvmCompiler::stringOf(LLValue* value) {
	string str;
	llvm::raw_string_ostream result(str);
	value->print(result);
	return result.str();
}

string LlvmCompiler::stringOf(LLType* type) {
	string str;
	llvm::raw_string_ostream result(str);
	type->print(result);
	return result.str();
}

LLValue* LlvmCompiler::findNamedValue(string name) {
	if (MapLocalVars.find(name) != MapLocalVars.end())
		return MapLocalVars[name];

	if (MapGlobalVars.find(name) != MapGlobalVars.end()){
		int index = MapGlobalVarsIndex[name];
		vector<LLValue*> valueIndex = makeIndexGEP({0, index});
		LLValue * basevalue = MapLocalVars["this"];
		LLValue * value = Builder.CreateGEP(basevalue, valueIndex);
		return value;
	}
	return nullptr;
}

string LlvmCompiler::getFunctionName(FunctionCall const* exp) {
	Expression const& baseExp = exp->expression();
	if (auto idExp = dynamic_cast<Identifier const*>(&baseExp)) {
		return ContractName + "." + idExp->name();
	}
	//exteral function call
	else if (auto exp = dynamic_cast<MemberAccess const*>(&baseExp)){
		string name = exp->memberName();
		Expression const& baseExp = exp->expression();
		TypePointer baseType = baseExp.annotation().type;
		return baseType->canonicalName()+"."+name;
	}

	return nullptr;
}

vector<LLValue*> LlvmCompiler::makeIndexGEP(list<int> indices) {
	vector<LLValue*> llIndices;
	for (auto i : indices)
		llIndices.push_back(LLConstantInt::get(LLType::getInt32Ty(Context), i));
	return llIndices;
}
