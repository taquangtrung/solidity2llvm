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


string LlvmCompiler::llvmString(const ContractDefinition* contract,
																StringMap sourceCodes) {
	// llvm::IRBuilder<> Builder1(Context);
	LoopStack.empty();

	cout << endl << "==========================" << endl ;
	cout << "** Compiling Solidity to LLVM IR ..." << endl;

	compilingSourceCodes = sourceCodes;
	compileContract(contract);

	// validate module
	llvm::verifyModule(*Module);

	cout << endl << "==========================" << endl ;
	cout << "** Output LLVM IR: " << endl << endl;
	Module->print(llvm::outs(), nullptr);

	std::error_code EC;
	llvm::raw_fd_ostream OS("module", EC, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(&(*Module), OS);
	OS.flush();

	return "";

}

/********************************************************
 *               Compile Contract
 ********************************************************/

void LlvmCompiler::compileContract(const ContractDefinition* contract) {
	// prepare environment
	MapGlobalVars.clear();

	// make contract
	ContractName = contract->name();
	Module = llvm::make_unique<llvm::Module>(ContractName, Context);

	// structs and enums
	for (const StructDefinition* d: contract->definedStructs())
		compileStructDecl(d);
	for (const EnumDefinition* d: contract->definedEnums())
		compileEnumDecl(d);

	// perform optimization passes
	FunctionPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(Module.get());

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

	for (const VariableDeclaration* var: contract->stateVariables())
		compileGlobalVarDecl(var);

	// functions
	for (const FunctionDefinition* func: contract->definedFunctions())
		compileFuncDecl(func);
}


/********************************************************
 *                Compile Declarations
 ********************************************************/

LLStructType* LlvmCompiler::compileStructDecl(const StructDefinition* d) {
	string llStructName = ContractName + "." + d->name();

	// fields of structs
	vector<LLType*> llElements;
	for (auto var : d->members())
		llElements.push_back(compileType(var->type()));

	LLStructType* llStructType = LLStructType::create(Context, llElements,
																										llStructName, true);
	MapStructTypes[llStructName] = llStructType;

	return llStructType;
}

LLIntegerType* LlvmCompiler::compileEnumDecl(const EnumDefinition* d) {
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


LLValue* LlvmCompiler::compileGlobalVarDecl(const VariableDeclaration* var) {
	LLType* llType = compileType(var->type());
	string name = var->name();

	LLConstant *llInitValue = nullptr;

	if (Expression* v = var->value().get())
		llInitValue = llvm::dyn_cast<LLConstant>(compileExp(v));

	auto llVar = new LLGlobalVar(*Module, llType, false,
															 LLGlobalVar::CommonLinkage,
															 llInitValue, name);

	MapGlobalVars[name] = llVar;
	SetGlobalVars.insert(llVar);

	return llVar;
}

LLValue* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var) {

	LLType* llType = compileType(var.type());
	string name = var.name();

	LLValue* llVar = Builder.CreateAlloca(llType, nullptr, name);
	MapLocalVars[name] = llVar;
	SetLocalVars.insert(llVar);

	return llVar;
}

LLValue* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var,
																					 const Expression* value) {
	auto llVar = compileLocalVarDecl(var);
	auto llValue = compileExp(value);
	return Builder.CreateStore(llValue, llVar);
}

LLFunction* LlvmCompiler::compileFuncDecl(const FunctionDefinition* func) {
	// prepare environment
	MapLocalVars.clear();

	// function name
	string funcName = func->name();

	// function type
	FunctionTypePointer funcType = func->functionType(false);
	auto returnTypes = funcType->returnParameterTypes();
	LLType* llRetType;
	if (returnTypes.size() == 0)
		llRetType = LLType::getVoidTy(Context);
	else if (returnTypes.size() == 1)
		llRetType = compileType(returnTypes.at(0));
	else {
		// TODO: handle returned type tuple
		LogError("CompileFunc: unknown returned function type");
	}

	// parameters
	vector<LLType*> llParamTypes;
	auto params = func->parameters();
	for (auto p: params)
		llParamTypes.push_back(compileType(p->type()));

	LLFunctionType* llFType = LLFunctionType::get(llRetType, llParamTypes, false);

	// create function
	LLFunction *llFunc = LLFunction::Create(llFType, LLFunction::CommonLinkage,
																					funcName, Module.get());

	// set names for parameters and also record it to local names
	int index = 0;
	for (auto &arg : llFunc->args()) {
		string paramName = params.at(index)->name();
		arg.setName(paramName);
		MapLocalVars[paramName] = &arg;
		index++;
	}

	// translate new function
	LLBlock *llBlock = LLBlock::Create(Context, "entry", llFunc);
	Builder.SetInsertPoint(llBlock);

	for (auto stmt: func->body().statements())
		compileStmt(*stmt);

	// validate function
	llvm::verifyFunction(*llFunc);

	// run optimization passes
	FunctionPM->run(*llFunc);

	return llFunc;
}

/********************************************************
 *                 Compile Statements
 ********************************************************/

void LlvmCompiler::compileStmt(Statement const& stmt) {
	if (auto s = dynamic_cast<InlineAssembly const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<Block const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<PlaceholderStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<IfStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<ForStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<WhileStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<Continue const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<Break const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<Return const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<Throw const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<EmitStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<VariableDeclarationStatement const*>(&stmt)) {
		return compileStmt(s);
	}
	else if (auto s = dynamic_cast<ExpressionStatement const*>(&stmt)) {
		return compileStmt(s);
	}
}

void LlvmCompiler::compileStmt(InlineAssembly const* stmt) {
	// TODO
	LogError("compileStmt: InlineAssembly: unhandled");
}

void LlvmCompiler::compileStmt(Block const* stmt) {
	LLFunction* llFunc = Builder.GetInsertBlock()->getParent();

	LLBlock* llBlock = LLBlock::Create(Context, "block", llFunc);
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

	LLBlock* llBlockThen = LLBlock::Create(Context, "if.then", llFunc);
	LLBlock* llBlockElse = LLBlock::Create(Context, "if.else", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, "if.end", llFunc);

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

	LLBlock* llBlockCond = LLBlock::Create(Context, "while.cond", llFunc);
	LLBlock* llBlockBody = LLBlock::Create(Context, "while.body", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, "while.end", llFunc);

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

	LLBlock* llBlockCond = LLBlock::Create(Context, "for.cond", llFunc);
	LLBlock* llBlockLoop = LLBlock::Create(Context, "for.loop", llFunc);
	LLBlock* llBlockBody = LLBlock::Create(Context, "for.body", llFunc);
	LLBlock* llBlockEnd = LLBlock::Create(Context, "for.end", llFunc);

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
	Builder.CreateRet(compileExp(stmt->expression()));
}

void LlvmCompiler::compileStmt(Throw const* stmt) {
	LogWarning("The 'throw' statement is deprecated from the version 0.4.13 ");
	return;
}

void LlvmCompiler::compileStmt(EmitStatement const* stmt) {
	compileExp(&(stmt->eventCall()));
}

void LlvmCompiler::compileStmt(VariableDeclarationStatement const* stmt) {
	auto vars = stmt->declarations();
	string result = "";
	if (vars.size() == 1)
		compileLocalVarDecl(*(vars.at(0)), stmt->initialValue());
	else
		LogError("compileStmt: VariableDeclarationStatement: more than 1 var");
}

void LlvmCompiler::compileStmt(ExpressionStatement const* stmt) {
	compileExp(&(stmt->expression()));
}

/********************************************************
 *                Compile Expressions
 ********************************************************/

LLValue* LlvmCompiler::compileExp(Expression const* exp) {
	if (auto e = dynamic_cast<Conditional const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<Assignment const*>(exp)) {
	  return compileExp(e);
	}
	else if (auto e = dynamic_cast<TupleExpression const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<UnaryOperation const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<BinaryOperation const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<FunctionCall const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<NewExpression const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<MemberAccess const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<IndexAccess const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<PrimaryExpression const*>(exp)) {
		return compileExp(e);
	}

	LogError("compileExp: unknown expression: ", *exp);
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Conditional const* exp) {
	// TODO
	LogError("compileExp: Conditional: unhandled");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Assignment const* exp) {
	LLValue* llLhs = compileExp(&(exp->leftHandSide()));
	LLValue* llRhs = compileExp(&(exp->rightHandSide()));
	return Builder.CreateStore(llRhs, llLhs);
}

LLValue* LlvmCompiler::compileExp(TupleExpression const* exp) {
	// TODO
	LogError("compileExp: TupleExpression: unhandled");
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
		LLValue* llResult = Builder.CreateAdd(llSubExp, llOne);
		return Builder.CreateStore(llResult, llSubExp);
	}

	case Token::Dec: {
		LLValue* llOne = LLConstantInt::get(llSubExp->getType(), 1);
		LLValue* llResult = Builder.CreateSub(llSubExp, llOne);
		return Builder.CreateStore(llResult, llSubExp);
	}

	case Token::Delete:
		LogError("compileExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("compileExp: UnaryOp: unknown operator: ", *exp);
		return nullptr;
	}
}

LLValue* LlvmCompiler::compileExp(BinaryOperation const* exp) {
	LLValue* llLhs = compileExp(&(exp->leftExpression()));
	LLValue* llRhs = compileExp(&(exp->rightExpression()));

	if (!llLhs || !llRhs) return nullptr;

	switch (exp->getOperator()) {
	case Token::Equal:
		return Builder.CreateICmpEQ(llLhs, llRhs);

	case Token::NotEqual:
		return Builder.CreateICmpNE(llLhs, llRhs);

	case Token::LessThan:
		return Builder.CreateICmpSLT(llLhs, llRhs);

	case Token::GreaterThan:
		return Builder.CreateICmpSGT(llLhs, llRhs);

	case Token::LessThanOrEqual:
		return Builder.CreateICmpSLE(llLhs, llRhs);

	case Token::GreaterThanOrEqual:
		return Builder.CreateICmpSGE(llLhs, llRhs);

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
		LogError("compileExp: BinaryOp: unknown operator: ", *exp);
		return nullptr;
	}
}

LLValue* LlvmCompiler::compileExp(FunctionCall const* exp) {
	FunctionCallAnnotation &annon = exp->annotation();
	LLType* llType = compileType(annon.type);

	if (annon.kind == FunctionCallKind::FunctionCall) {
		LogDebug("compileExp: Normal FunctionCall");

		string funcName = *(exp->names().at(0));
		LLFunction *llFunc = Module->getFunction(funcName);

		vector<LLValue*> llArgs;
		for (auto arg : exp->arguments())
			llArgs.push_back(compileExp((&arg)->get()));

		return Builder.CreateCall(llFunc, llArgs);
	}

	else if (annon.kind == FunctionCallKind::StructConstructorCall) {
		vector<LLConstant*> llArgs;
		for (auto arg : exp->arguments())
			if (auto a = llvm::dyn_cast<LLConstant>(compileExp((&arg)->get())))
				llArgs.push_back(a);

		if (auto llStructType = llvm::dyn_cast<LLStructType>(llType))
			return llvm::ConstantStruct::get(llStructType, llArgs);
	}

	else if (annon.kind == FunctionCallKind::TypeConversion) {
		LLValue* llArg = compileExpArgument((&(exp->arguments().at(0)))->get());

		if (auto t = dynamic_cast<IntegerType const*>(annon.type)) {
			if (t->isSigned())
				return Builder.CreateSExtOrTrunc(llArg, llType);
			else
				return Builder.CreateZExtOrTrunc(llArg, llType);
		}
		else
			return Builder.CreateZExtOrTrunc(llArg, llType);
	}

	else {
		LogError("compileExp: FunctionCall: unknown FunctionCall type");
		return nullptr;
	}
}

LLValue* LlvmCompiler::compileExp(NewExpression const* exp) {
	// TODO: need to implement
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(MemberAccess const* exp) {
	TypePointer coreType = exp->annotation().type;

	if (auto type = dynamic_cast<EnumType const*>(coreType)) {
		string llEnumName = type->canonicalName();
		map<string, int> llMemberValues = MapEnumTypes[llEnumName];
		int value = llMemberValues[exp->memberName()];
		LLIntegerType* llIntType = LLIntegerType::get(Context, 64);
		return LLConstantInt::get(llIntType, value);
	}

	LogError("compileExp: MemberAccess: unknown exp");

	// TODO
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(IndexAccess const* exp) {
	// TODO
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(PrimaryExpression const* exp) {
	if (auto e = dynamic_cast<Identifier const*>(exp)) {
		compileExp(e);
	}
	else if (auto e = dynamic_cast<ElementaryTypeNameExpression const*>(exp)) {
		return compileExp(e);
	}
	else if (auto e = dynamic_cast<Literal const*>(exp)) {
		return compileExp(e);
	}

	LogError("compileExp: PrimaryExpression: unknown expression");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Identifier const *exp) {
	// LLValue* llexp = findNamedValue(exp->name());
	// return Builder.CreateLoad(llexp);
	return findNamedValue(exp->name());
}

LLValue* LlvmCompiler::compileExp(ElementaryTypeNameExpression const *exp) {
	LogError("compileExp: ElementaryTypeNameExpression: unhandled");
	return nullptr;
}

LLValue* LlvmCompiler::compileExp(Literal const *exp) {
	// Literal types can be one of the following:
	// TrueLiteral, FalseLiteral, Number, StringLiteral, and CommentLiteral
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
		LogError("compileExp: Literal: unknown token: ", *exp);
		return nullptr;
	}
}

/********************************************************
 *                Auxiliary LLVM functions
 ********************************************************/

LLValue* LlvmCompiler::compileExpArgument(Expression const* exp) {
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
	if (auto t = dynamic_cast<ElementaryTypeName const*>(type)) {
		return compileTypeName(t);
	}
	else if (auto t = dynamic_cast<UserDefinedTypeName const*>(type)) {
		return compileTypeName(t);
	}
	else if (auto t = dynamic_cast<FunctionTypeName const*>(type)) {
		return compileTypeName(t);
	}
	else if (auto t = dynamic_cast<Mapping const*>(type)) {
		return compileTypeName(t);
	}
	else if (auto t = dynamic_cast<ArrayTypeName const*>(type)) {
		return compileTypeName(t);
	}

	LogError("compileTypeName: Unknown TypeName: ", type);
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
	// TODO
	LogError("compileTypeName: Mapping: unhanded");
	return nullptr;
}

LLType* LlvmCompiler::compileTypeName(ArrayTypeName const* type) {
	// TODO
	LogError("compileTypeName: ArrayTypeName: unhandled");
	return nullptr;
}

LLType* LlvmCompiler::compileType(TypePointer type) {
	if (type == nullptr)
		LogError("compileType: input is null type");
	else if (auto t = dynamic_cast<IntegerType const*>(type)) {
		return LLIntegerType::get(Context, t->numBits());
	}
	else if (auto t = dynamic_cast<FixedPointType const*>(type)) {
		LogError("FixedPointType");
	}
	else if (auto t = dynamic_cast<RationalNumberType const*>(type)) {
		LogError("RationalNumberType");
	}
	else if (auto t = dynamic_cast<StringLiteralType const*>(type)) {
		LogError("StringLiteralType");
	}
	else if (auto t =  dynamic_cast<BoolType const*>(type)) {
		return LLIntegerType::get(Context, 8);
	}
	else if (auto t = dynamic_cast<StructType const*>(type)) {
		return MapStructTypes[t->canonicalName()];
	}
	else if (auto t = dynamic_cast<FixedBytesType const*>(type)) {
		uint64_t size = t->numBytes();
		LLIntegerType* llBaseType = LLIntegerType::get(Context, 8);
		return LLArrayType::get(llBaseType, size);
	}
	else if (auto t = dynamic_cast<ArrayType const*>(type)) {
		uint64_t size = (uint64_t)t->length();  // converting u256 to uint64
		LLType* llBaseType = compileType(t->baseType());
		return LLArrayType::get(llBaseType, size);
	}
	else if (auto t = dynamic_cast<ContractType const*>(type)) {
		LogError("ContractType");
	}
	else if (auto t = dynamic_cast<EnumType const*>(type)) {
		// compile an enum type to an integer type
		return LLIntegerType::get(Context, 64);
	}
	else if (auto t = dynamic_cast<TupleType const*>(type)) {
		vector<TypePointer> components = t->components();
		cout << "TupleType: num of components: " << components.size();
		LogError("TupleType");
	}
	else if (auto t = dynamic_cast<FunctionType const*>(type)) {
		LogError("FunctionType");
	}
	else if (auto t = dynamic_cast<MappingType const*>(type)) {
		LogError("MappingType");
	}
	else if (auto t = dynamic_cast<TypeType const*>(type)) {
		LogError("TypeType");
	}
	else if (auto t = dynamic_cast<ModifierType const*>(type)) {
		LogError("ModifierType");
	}
	else if (auto t = dynamic_cast<ModuleType const*>(type)) {
		LogError("ModuleType");
	}
	else if (auto t = dynamic_cast<MagicType const*>(type)) {
		LogError("MagicType");
	}
	else if (auto t = dynamic_cast<InaccessibleDynamicType const*>(type)) {
		LogError("InaccessibleDynamicType");
	}
	else if (auto t = dynamic_cast<AddressType const*>(type)) {
		// Soldity's address is 20 bytes
		auto byteType = LLIntegerType::get(Context, 8);
		return LLArrayType::get(byteType, 20);
	}

	LogError("compileType: Unknown Type", type);

	return nullptr;
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
	else if (MapGlobalVars.find(name) != MapGlobalVars.end())
		return MapGlobalVars[name];
	else return nullptr;
}
