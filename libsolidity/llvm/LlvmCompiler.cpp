#include <iostream>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/LlvmCompiler.h"
#include <libsolidity/interface/Exceptions.h>
#include <libdevcore/SHA3.h>


namespace legacy = llvm::legacy;

using namespace std;
using namespace dev;
using namespace dev::solidity;




bool debug = true;

void LogError(const char *msg) {
	fprintf(stderr, "\n!!!Error: %s\n", msg);
	exit (1);
}

void LogError(const char *msg, ASTNode const& node) {
	ASTPrinter printer(node);
	std::cerr << "\n!!! Error: " << msg << "\n";
	printer.print(std::cerr);
	std::cerr << endl;
	exit (1);
}

void LogWarning(const char *msg) {
	fprintf(stderr, "\n!!! Warning: %s\n", msg);
}

void LogDebug(string msg) {
	if (debug)
		cout << "!! Debug: " << msg << endl;
}

void LogDebug(string msg, ASTNode const& node) {
	if (debug) {
		llvm::outs() << "!! Debug: " << msg;
		ASTPrinter printer(node);
		printer.print(std::cerr);
		std::cerr << endl;
	}
}

void LogDebug(string msg, llvm::Value* value) {
	if (debug) {
		llvm::outs() << "!! Debug: " << msg;
		value->print(llvm::outs());
		llvm::outs() << "\n";
	}
}

void LogDebug(string msg, llvm::Type* type) {
	if (debug) {
		llvm::outs() << "!! Debug: " << msg;
		type->print(llvm::outs());
		llvm::outs() << "\n";
	}
}

/*
  _sourceCodes: maps name of a contract to its source code
*/
string LlvmCompiler::llvmString(const ContractDefinition* contract, StringMap sourceCodes) {
	// llvm::IRBuilder<> Builder1(Context);
	LoopStack.empty();


	cout << "start to compile to LLVM IR..." << endl;

	compilingSourceCodes = sourceCodes;

	compileContract(contract);

	// validate module
	llvm::verifyModule(*Module);


	cout << "====== OUTPUT LLVM IR ======" << endl << endl;
	Module->print(llvm::outs(), nullptr);

	std::error_code EC;
	llvm::raw_fd_ostream OS("module", EC, llvm::sys::fs::F_None);
	llvm::WriteBitcodeToFile(&(*Module), OS);
	OS.flush();

	return "";

}

// string LlvmCompiler::compileStmt(Block const* stmt, int indent) {
//    string strIndent = createIndent(indent);
//    string result = strIndent + "{\n" ;
//    for (auto s : stmt->statements())
//      result = result + compileStmt(*s, indent + 1) + "\n";
//    result = result + strIndent + "}" ;
//    return result;
// }

// string LlvmCompiler::compileStmt(WhileStatement const* stmt, int indent) {
//    string strIndent = createIndent(indent);
//    string strCond = compileExp(&(stmt->condition()));
//    string strBody = compileStmt(stmt->body(), indent);
//    string result = strIndent + "while (" + strCond + ")\n" + strBody;
//    return result;
// }

// string LlvmCompiler::compileStmt(ForStatement const* stmt, int indent) {
//    string strIndent = createIndent(indent);
//    string strInit = compileStmt(*(stmt->initializationExpression()), 0);
//    string strLoop = compileStmt(stmt->loopExpression(), 0);
//    string strCond = compileExp(stmt->condition());
//    string strBody = compileStmt(stmt->body(), indent);
//    string result = strIndent + "for (" + strInit + "; " + strCond +
//      "; " + strLoop + ")\n" + strBody;
//    return result;
// }

// string LlvmCompiler::compileExp(FunctionCall const* exp) {
//    string strFunc = compileExp(&(exp->expression()));
//    vector<string> strArgs;
//    for (auto arg : exp->arguments())
//      strArgs.push_back(compileExp((&arg)->get()));
//    string strArg = boost::algorithm::join(strArgs, ", ");
//    string result = strFunc + "(" + strArg + ")";
//    // if (debug)
//    //    result = "(FunctionCall: " + result + ")";
//    return result;
// }

// string LlvmCompiler::compileExp(NewExpression const* exp) {
//    // return "new " + compileTypeName(exp->typeName());
//    return "new";
// }

// string LlvmCompiler::compileExp(MemberAccess const* exp) {
//    string strBase = compileExp(&(exp->expression()));
//    string strMember = exp->memberName();
//    return strBase + "." + strMember;
// }

// string LlvmCompiler::compileExp(IndexAccess const* exp) {
//    string strBase = compileExp(&(exp->baseExpression()));
//    string strIndex = compileExp(exp->indexExpression());
//    return strBase + "[" + strIndex + "]";
// }

/********************************************************
 *               Compile Contract
 ********************************************************/

void LlvmCompiler::compileContract(const ContractDefinition* contract) {
	// prepare environment
	GlobalNamedValues.clear();

	// structs
	for (const StructDefinition* st: contract->definedStructs())
		compileStructDecl(st);

	// // enum
	// // for (const EnumDefinition* en: contract->definedStructs())
	// //   result = result + "\n\n" + compileStruct(st);

	// make contract
	string contractName = contract->name();
	Module = llvm::make_unique<llvm::Module>(contractName, Context);

	// perform optimization passes
	FunctionPM = llvm::make_unique<legacy::FunctionPassManager>(Module.get());
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

	// global variables
	for (const VariableDeclaration* var: contract->stateVariables())
		compileGlobalVarDecl(var);

	// functions
	for (const FunctionDefinition* func: contract->definedFunctions())
		compileFunc(func);


}


/********************************************************
 *                Compile Declarations
 ********************************************************/

llvm::StructType* LlvmCompiler::compileStructDecl(const StructDefinition* st) {
	string name = st->name();
	cout << "Struct Name: " << st->fullyQualifiedName() << endl;
	vector<llvm::Type*> elements;
	for (auto var : st->members()) {
		elements.push_back(compileTypeName(var->typeName()));
	}
	auto llvmStruct = llvm::StructType::create(Context, elements, name, true);
	NamedStructTypes[name] = llvmStruct;
	return llvmStruct;
}


Value* LlvmCompiler::compileGlobalVarDecl(const VariableDeclaration* var) {
	llvm::Type* type = compileType(var->type());
	// Value* initVal = compileExp(var->value().get());
	string name = var->name();

	llvm::GlobalVariable* llvmVar =
		new llvm::GlobalVariable(*Module, type, false,
								 llvm::GlobalVariable::CommonLinkage,
								 nullptr, name);
	GlobalNamedValues[name] = llvmVar;
	return llvmVar;
}

Value* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var) {

	llvm::Type* type = compileType(var.type());
	string name = var.name();

	auto llvmVar = Builder.CreateAlloca(type, nullptr, name);
	LocalNamedValues[name] = llvmVar;

	return llvmVar;
}

Value* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var,
                                         const Expression* value) {
	auto llvmVar = compileLocalVarDecl(var);
	auto llvmValue = compileExp(value);
	return Builder.CreateStore(llvmValue, llvmVar);
}

llvm::Function* LlvmCompiler::compileFunc(const FunctionDefinition* func) {
	// prepare environment
	LocalNamedValues.clear();

	// function name
	string funcName = func->name();

	// function type
	FunctionTypePointer funcType = func->functionType(false);
	auto returnTypes = funcType->returnParameterTypes();
	llvm::Type* llvmRetType;
	if (returnTypes.size() == 0)
		llvmRetType = llvm::Type::getVoidTy(Context);
	else if (returnTypes.size() == 1)
		llvmRetType = compileType(returnTypes.at(0));
	else {
		// TODO: handle returned type tuple
		LogError("CompileFunc: unknown returned function type");
	}
	vector<llvm::Type*> llvmParamTypes;
	auto params = func->parameters();
	for (auto p: params)
		llvmParamTypes.push_back(compileType(p->type()));
	llvm::FunctionType* llvmFuncType =
		llvm::FunctionType::get(llvmRetType, llvmParamTypes, false);

	// create function
	llvm::Function *llvmFunc =
		llvm::Function::Create(llvmFuncType, llvm::Function::CommonLinkage,
							   funcName, Module.get());

	// set names for parameters and also record it to local names
	int index = 0;
	for (auto &arg : llvmFunc->args()) {
		string paramName = params.at(index)->name();
		arg.setName(paramName);
		LocalNamedValues[paramName] = &arg;
		index++;
	}

	// translate new function
	BasicBlock *block = BasicBlock::Create(Context, "entry", llvmFunc);
	Builder.SetInsertPoint(block);

	for (auto stmt: func->body().statements())
		compileStmt(*stmt);

	// validate function
	llvm::verifyFunction(*llvmFunc);

	// run optimization passes
	FunctionPM->run(*llvmFunc);

	return llvmFunc;
}

/********************************************************
 *                 Compile Statements
 ********************************************************/

void LlvmCompiler::compileStmt(Statement const& stmt) {
	if (auto s = dynamic_cast<InlineAssembly const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<Block const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<PlaceholderStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<IfStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<ForStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<WhileStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<Continue const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<Break const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<Return const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<Throw const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<EmitStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<VariableDeclarationStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
	if (auto s = dynamic_cast<ExpressionStatement const*>(&stmt)) {
		if (s != nullptr) return compileStmt(s);
	}
}

void LlvmCompiler::compileStmt(InlineAssembly const* stmt) {
	// TODO
	LogError("compileStmt: InlineAssembly: unhandled");
}

void LlvmCompiler::compileStmt(Block const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* block = BasicBlock::Create(Context, "block", llvmFunc);
	Builder.SetInsertPoint(block);

	for (auto s : stmt->statements())
		compileStmt(*s);
}

void LlvmCompiler::compileStmt(PlaceholderStatement const* stmt) {
	// TODO
	LogError("compileStmt: PlaceholderStatement: unhandled");
}

void LlvmCompiler::compileStmt(IfStatement const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* thenBlock = BasicBlock::Create(Context, "if.then", llvmFunc);
	BasicBlock* elseBlock = BasicBlock::Create(Context, "if.else", llvmFunc);
	BasicBlock* endBlock = BasicBlock::Create(Context, "if.end", llvmFunc);

	Value* condValue = compileExp(&(stmt->condition()));
	Builder.CreateCondBr(condValue, thenBlock, elseBlock);

	Builder.SetInsertPoint(thenBlock);
	compileStmt(stmt->trueStatement());
	Builder.CreateBr(endBlock);

	auto elseStmt = stmt->falseStatement();
	if (elseStmt != nullptr) {
		Builder.SetInsertPoint(elseBlock);
		compileStmt(*elseStmt);
		Builder.CreateBr(endBlock);

		Builder.SetInsertPoint(endBlock);
		// llvm::Type* phiType =  thenValue->getType();
		// llvm::PHINode *phiNode = Builder.CreatePHI(phiType, 2, "if_stmt");
		// LogDebug("ThenBlock: ", thenBlock);
		// LogDebug("ThenValue: ", thenValue);
		// LogDebug("ElseBlock: ", elseBlock);
		// LogDebug("ElseValue: ", elseValue);
		// LogDebug("ElseType: ", elseValue->getType());
		// LogDebug("PhiType: ", phiType);
		// phiNode->addIncoming(thenValue, thenBlock);
		// phiNode->addIncoming(elseValue, elseBlock);
		// return phiNode;
	}
}

void LlvmCompiler::compileStmt(WhileStatement const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* condBlock = BasicBlock::Create(Context, "while.cond", llvmFunc);
	BasicBlock* bodyBlock = BasicBlock::Create(Context, "while.body", llvmFunc);
	BasicBlock* endBlock = BasicBlock::Create(Context, "while.end", llvmFunc);

	// store to the loop stack
	LoopInfo loop {condBlock, endBlock};
	LoopStack.push(loop);

	// loop condition
	Builder.SetInsertPoint(condBlock);
	Value* condValue = compileExp(&(stmt->condition()));
	Builder.CreateCondBr(condValue, bodyBlock, endBlock);

	// loop body
	Builder.SetInsertPoint(bodyBlock);
	compileStmt(stmt->body());
	Builder.CreateBr(condBlock);

	// end block
	Builder.SetInsertPoint(endBlock);

	// remove the loop from the stack
	LoopStack.pop();
}

void LlvmCompiler::compileStmt(ForStatement const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* condBlock = BasicBlock::Create(Context, "for.cond", llvmFunc);
	BasicBlock* loopBlock = BasicBlock::Create(Context, "for.loop", llvmFunc);
	BasicBlock* bodyBlock = BasicBlock::Create(Context, "for.body", llvmFunc);
	BasicBlock* endBlock = BasicBlock::Create(Context, "for.end", llvmFunc);

	// store to the loop stack
	LoopInfo loop {condBlock, endBlock};
	LoopStack.push(loop);

	// loop initialization
	compileStmt(*(stmt->initializationExpression()));
	Builder.CreateBr(condBlock);

	// loop condition
	Builder.SetInsertPoint(condBlock);
	Value* condValue = compileExp(stmt->condition());
	Builder.CreateCondBr(condValue, bodyBlock, endBlock);

	// loop expression
	Builder.SetInsertPoint(loopBlock);
	compileStmt(stmt->loopExpression());
	Builder.CreateBr(condBlock);

	// loop body
	Builder.SetInsertPoint(bodyBlock);
	compileStmt(stmt->body());
	Builder.CreateBr(loopBlock);

	// end block
	Builder.SetInsertPoint(endBlock);

	// remove the loop from the stack
	LoopStack.pop();
}

void LlvmCompiler::compileStmt(Continue const* stmt) {
	if (LoopStack.empty())
		LogError("compileStmt: Continue: empty Loop Stack");

	LoopInfo loop = LoopStack.top();
	Builder.CreateBr(loop.loopHead);
}

void LlvmCompiler::compileStmt(Break const* stmt) {
	if (LoopStack.empty())
		LogError("compileStmt: Break: empty Loop Stack");

	LoopInfo loop = LoopStack.top();
	Builder.CreateBr(loop.loopEnd);
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

Value* LlvmCompiler::compileExp(Expression const* exp) {
	if (auto e = dynamic_cast<Conditional const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<Assignment const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<TupleExpression const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<UnaryOperation const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<BinaryOperation const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<FunctionCall const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<NewExpression const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<MemberAccess const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<IndexAccess const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<PrimaryExpression const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	LogDebug("compileExp: unknown expression: ", *exp);
	LogError("compileExp: unknown expression: ", *exp);
	return nullptr;
}

Value* LlvmCompiler::compileExp(Conditional const* exp) {
	// TODO
	LogError("compileExp: Conditional: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileExp(Assignment const* exp) {
	Value* lhs = compileExp(&(exp->leftHandSide()));
	Value* rhs = compileExp(&(exp->rightHandSide()));
	if (lhs == nullptr)
		LogError("compileExp: Assignment: null lhs");
	if (rhs == nullptr)
		LogError("compileExp: Assignment: null rhs");
	return Builder.CreateStore(rhs, lhs);
}

Value* LlvmCompiler::compileExp(TupleExpression const* exp) {
	// TODO
	LogError("compileExp: TupleExpression: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileExp(UnaryOperation const* exp) {
	Value* subExp = compileExp(&(exp->subExpression()));
	if (!subExp) return nullptr;

	switch (exp->getOperator()) {
	case Token::Not:
		return Builder.CreateNot(subExp, "");

	case Token::BitNot:
		return Builder.CreateNot(subExp, "");

	case Token::Inc: {
		Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateAdd(subExp, one, "");
		return Builder.CreateStore(newExp, subExp);
	}

	case Token::Dec: {
		Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateSub(subExp, one, "");
		return Builder.CreateStore(newExp, subExp);
	}

	case Token::Delete:
		LogError("compileExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("compileExp: UnaryOp: unknown operator: ", *exp);
		return nullptr;
	}
}

Value* LlvmCompiler::compileExp(BinaryOperation const* exp) {
	Value* lhs = compileExp(&(exp->leftExpression()));
	Value* rhs = compileExp(&(exp->rightExpression()));
	if (!lhs || !rhs) return nullptr;

	switch (exp->getOperator()) {
	case Token::Equal:
		return Builder.CreateICmpEQ(lhs, rhs, "");

	case Token::NotEqual:
		return Builder.CreateICmpNE(lhs, rhs, "");

	case Token::LessThan:
		return Builder.CreateICmpSLT(lhs, rhs, "");

	case Token::GreaterThan:
		return Builder.CreateICmpSGT(lhs, rhs, "");

	case Token::LessThanOrEqual:
		return Builder.CreateICmpSLE(lhs, rhs, "");

	case Token::GreaterThanOrEqual:
		return Builder.CreateICmpSGE(lhs, rhs, "");

	case Token::Comma:
		LogError("compileExp: BinaryOp: need to support Comma Exp");
		return nullptr;

	case Token::Or:
		return Builder.CreateOr(lhs, rhs, "");

	case Token::And:
		return Builder.CreateOr(lhs, rhs, "");

	case Token::BitOr:
		return Builder.CreateOr(lhs, rhs, "");

	case Token::BitXor:
		return Builder.CreateXor(lhs, rhs, "");

	case Token::BitAnd:
		return Builder.CreateXor(lhs, rhs, "");

	case Token::SHL:
		return Builder.CreateShl(lhs, rhs, "");

	case Token::SAR:
		LogError("compileExp: BinaryOp: need to support SAR");
		return nullptr;

	case Token::SHR:
		return Builder.CreateLShr(lhs, rhs, "");

	case Token::Add:
		return Builder.CreateAdd(lhs, rhs, "");

	case Token::Sub:
		return Builder.CreateSub(lhs, rhs, "");

	case Token::Mul:
		return Builder.CreateMul(lhs, rhs, "");

	case Token::Div:
		return Builder.CreateUDiv(lhs, rhs, "");

	case Token::Mod:
		return Builder.CreateURem(lhs, rhs, "");

	case Token::Exp:
		LogError("compileExp: BinaryOp: unhandled Exponential Exp");
		return nullptr;

	default:
		LogError("compileExp: BinaryOp: unknown operator: ", *exp);
		return nullptr;
	}
}

Value* LlvmCompiler::compileExp(FunctionCall const* exp) {
	string funcName = *(exp->names().at(0));
	cout << "FuncCall: FuncName: " << funcName << endl;
	llvm::Function *callee = Module->getFunction(funcName);

	vector<Value*> arguments;
	for (auto arg : exp->arguments())
		arguments.push_back(compileExp((&arg)->get()));

	if (callee->arg_size() != arguments.size()) {
		LogError("compileExp: FunctionCall: mistmatch arguments");
		return nullptr;
	}

	return Builder.CreateCall(callee, arguments, "functioncall");
}

Value* LlvmCompiler::compileExp(NewExpression const* exp) {
	// TODO
	return nullptr;
}

Value* LlvmCompiler::compileExp(MemberAccess const* exp) {
	//    string strBase = compileExp(&(exp->expression()));
	//    string strMember = exp->memberName();
	//    return strBase + "." + strMember;
	Value* llvmBase = compileExp(&(exp->expression()));

	// TODO
	return nullptr;
}

Value* LlvmCompiler::compileExp(IndexAccess const* exp) {
	// TODO
	return nullptr;
}

Value* LlvmCompiler::compileExp(PrimaryExpression const* exp) {
	if (auto e = dynamic_cast<Identifier const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<ElementaryTypeNameExpression const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	if (auto e = dynamic_cast<Literal const*>(exp)) {
		if (e != nullptr) return compileExp(e);
	}
	LogError("compileExp: PrimaryExpression: unknown expression");
	return nullptr;
}

Value* LlvmCompiler::compileExp(Identifier const *exp) {
	return findNamedValue(exp->name());
}

Value* LlvmCompiler::compileExp(ElementaryTypeNameExpression const *exp) {
	// TODO
	LogError("compileExp: ElementaryTypeNameExpression: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileExp(Literal const *exp) {
	// Literal types can be one of the following:
	// TrueLiteral, FalseLiteral, Number, StringLiteral, and CommentLiteral
	string content = exp->value();
	switch (exp->token()) {

	case Token::StringLiteral:
		return llvm::ConstantDataArray::getString(Context, content);

	case Token::TrueLiteral:
		return llvm::ConstantInt::getTrue(Context);

	case Token::FalseLiteral:
		return llvm::ConstantInt::getFalse(Context);

	case Token::Number: {
		// FIXME: temporarily fix to 64 bit, signed integer
		llvm::IntegerType* intTyp = llvm::IntegerType::get(Context, 64);
		return llvm::ConstantInt::get(intTyp, atoi(content.data()), true);
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
 *                     Compile types
 ********************************************************/

llvm::Type* LlvmCompiler::compileTypeName(TypeName const* type) {
	if (auto t = dynamic_cast<ElementaryTypeName const*>(type)) {
		if (t != nullptr) return compileTypeName(t);
	}
	if (auto t = dynamic_cast<UserDefinedTypeName const*>(type)) {
		if (t != nullptr) return compileTypeName(t);
	}
	if (auto t = dynamic_cast<FunctionTypeName const*>(type)) {
		if (t != nullptr) return compileTypeName(t);
	}
	if (auto t = dynamic_cast<Mapping const*>(type)) {
		if (t != nullptr) return compileTypeName(t);
	}
	if (auto t = dynamic_cast<ArrayTypeName const*>(type)) {
		if (t != nullptr) return compileTypeName(t);
	}
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(ElementaryTypeName const* type) {
	TypePointer ty = Type::fromElementaryTypeName(type->typeName());
	return compileType(ty);
}

llvm::Type* LlvmCompiler::compileTypeName(UserDefinedTypeName const* type) {
	// TODO
	LogError("compileTypeName: UserDefinedTypeName: unhandled");
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(FunctionTypeName const* type) {
	// TODO
	LogError("compileTypeName: FunctionTypeName: unhandled");
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(Mapping const* type) {
	// TODO
	LogError("compileTypeName: Mapping: unhandled");
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(ArrayTypeName const* type) {
	// TODO
	LogError("compileTypeName: ArrayTypeName: unhandled");
	return nullptr;
}

llvm::Type* LlvmCompiler::compileType(TypePointer type) {
	if (auto t = dynamic_pointer_cast<IntegerType const>(type)) {
		if (t != nullptr)
			return llvm::IntegerType::get(Context, t->numBits());
	}
	else if (auto t = dynamic_pointer_cast<FixedPointType const>(type)) {
		if (t != nullptr) LogError("FixedPointType");
	}
	else if (auto t = dynamic_pointer_cast<RationalNumberType const>(type)) {
		if (t != nullptr) LogError("RationalNumberType");
	}
	else if (auto t = dynamic_pointer_cast<StringLiteralType const>(type)) {
		if (t != nullptr) LogError("StringLiteralType");
		// if (t != nullptr)
		// 	return llvm::IntegerType::get(Context, t->numBits());
	}
	else if (dynamic_pointer_cast<FixedBytesType const>(type)) {
		if (t != nullptr) LogError("FixedBytesType");
	}
	else if (auto t =  dynamic_pointer_cast<BoolType const>(type)) {
		if (t != nullptr)
			// consider BoolType as an IntType 8 bit
			return llvm::IntegerType::get(Context, 8);
	}
	else if (auto t = dynamic_pointer_cast<StructType const>(type)) {
		if (t != nullptr) {
			cout << "TypeName: " << t->canonicalName() << endl;
			LogError("StructType");
		}
	}
	else if (auto t = dynamic_pointer_cast<ArrayType const>(type)) {
		if (t != nullptr) LogError("ArrayType");
	}
	else if (auto t = dynamic_pointer_cast<ContractType const>(type)) {
		if (t != nullptr) LogError("ContractType");
	}
	else if (auto t = dynamic_pointer_cast<EnumType const>(type)) {
		if (t != nullptr) LogError("EnumType");
	}
	else if (auto t = dynamic_pointer_cast<TupleType const>(type)) {
		if (t != nullptr) LogError("TupleType");
	}
	else if (auto t = dynamic_pointer_cast<FunctionType const>(type)) {
		if (t != nullptr) LogError("FunctionType");
	}
	else if (auto t = dynamic_pointer_cast<MappingType const>(type)) {
		if (t != nullptr) LogError("MappingType");
	}
	else if (auto t = dynamic_pointer_cast<TypeType const>(type)) {
		if (t != nullptr) LogError("TypeType");
	}
	else if (auto t = dynamic_pointer_cast<ModifierType const>(type)) {
		if (t != nullptr) LogError("ModifierType");
	}
	else if (auto t = dynamic_pointer_cast<ModuleType const>(type)) {
		if (t != nullptr) LogError("ModuleType");
	}
	else if (auto t = dynamic_pointer_cast<MagicType const>(type)) {
		if (t != nullptr) LogError("MagicType");
	}
	else if (auto t = dynamic_pointer_cast<InaccessibleDynamicType const>(type)) {
		if (t != nullptr) LogError("InaccessibleDynamicType");
	}

	LogError("compileType: unknown type");
	return nullptr;
}


/********************************************************
 *                Supporting Functions
 ********************************************************/

string LlvmCompiler::stringOf(llvm::Module* module) {
	if (module == nullptr)
		return "(nullptr module)";

	string result = "";
	for (auto &func : module->getFunctionList())
		result = result + "\n**************\n\n" + stringOf(&func);
	return result;
}

string LlvmCompiler::stringOf(llvm::Function* func) {
	if (func == nullptr)
		return "(nullptr function)";

	string result = "";
	for (auto &block : func->getBasicBlockList()) {
		result = result + stringOf(&block) + "\n\n";
	}
	return result;
}

string LlvmCompiler::stringOf(BasicBlock* block) {
	if (block == nullptr)
		return "(nullptr block)";

	string result = "";
	for (auto &inst : block->getInstList()) {
		result = result + stringOf(&inst) + "\n";
	}
	return result;
}

string LlvmCompiler::stringOf(Value* value) {
	if (value == nullptr)
		return "(nullptr value)";

	string strValue;
	llvm::raw_string_ostream rso(strValue);
	value->print(rso);
	return strValue;
}

Value* LlvmCompiler::findNamedValue(string name) {
	if (LocalNamedValues.find(name) != LocalNamedValues.end())
		return LocalNamedValues[name];
	else if (GlobalNamedValues.find(name) != GlobalNamedValues.end())
		return GlobalNamedValues[name];
	else return nullptr;
}
