#include <iostream>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/LlvmCompiler.h"
#include <libsolidity/interface/Exceptions.h>
#include <libdevcore/SHA3.h>



using namespace std;
using namespace dev;
using namespace dev::solidity;

using Value = llvm::Value;

static llvm::LLVMContext Context;
static llvm::IRBuilder<> Builder(Context);
static std::unique_ptr<llvm::Module> CompilingModule;
static std::map<std::string, Value *> GlobalNamedValues;
static std::map<std::string, Value *> LocalNamedValues;

bool debug = true;


void LogError(const char *msg) {
	fprintf(stderr, "\n!!!Error: %s\n", msg);
	exit (1);
}

void LogDebug(string msg) {
	if (debug)
		cout << "!!Debug: " << msg << endl;
}

/*
   _sourceCodes: maps name of a contract to its source code
 */
string LlvmCompiler::llvmString(const ContractDefinition* contract, StringMap sourceCodes) {
	cout << "start to compile to LLVM IR..." << endl;

	compilingSourceCodes = sourceCodes;

	compileContract(contract);

	cout << "====== OUTPUT LLVM IR ======\n";
	CompilingModule->print(llvm::outs(), nullptr);

	return "";

}

// string LlvmCompiler::compileStructDecl(const StructDefinition* st) {
// 	string result = "struct " + st->name() + " {\n";
// 	string strIndent = createIndent(1);
// 	for (auto var : st->members())
// 		result = result + strIndent + compileTypeName(var->typeName())
// 			+ " " + var->name() + ";\n";
// 	result = result + "}";
// 	return result;
// }

// string LlvmCompiler::compileStmt(Block const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string result = strIndent + "{\n" ;
// 	for (auto s : stmt->statements())
// 		result = result + compileStmt(*s, indent + 1) + "\n";
// 	result = result + strIndent + "}" ;
// 	return result;
// }

// string LlvmCompiler::compileStmt(IfStatement const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string strCond = compileExp(&(stmt->condition()));
// 	string strTrue = compileStmt(stmt->trueStatement(), indent);
// 	Statement const* falseStmt = stmt->falseStatement();
// 	string result =
// 		strIndent + "if (" + strCond + ")\n" + strTrue;
// 	if (falseStmt != nullptr) {
// 		string strFalse = compileStmt(*(falseStmt), indent);
// 		result = result + strIndent + "else\n" + strFalse;
// 	}
// 	return result;
// }

// string LlvmCompiler::compileStmt(BreakableStatement const* stmt, int indent) {
// 	if (auto s = dynamic_cast<WhileStatement const*>(stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<ForStatement const*>(stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	return "(Unknown BreakableStatement)";
// }

// string LlvmCompiler::compileStmt(WhileStatement const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string strCond = compileExp(&(stmt->condition()));
// 	string strBody = compileStmt(stmt->body(), indent);
// 	string result = strIndent + "while (" + strCond + ")\n" + strBody;
// 	return result;
// }

// string LlvmCompiler::compileStmt(ForStatement const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string strInit = compileStmt(*(stmt->initializationExpression()), 0);
// 	string strLoop = compileStmt(stmt->loopExpression(), 0);
// 	string strCond = compileExp(stmt->condition());
// 	string strBody = compileStmt(stmt->body(), indent);
// 	string result = strIndent + "for (" + strInit + "; " + strCond +
// 		"; " + strLoop + ")\n" + strBody;
// 	return result;
// }

// string LlvmCompiler::compileExp(FunctionCall const* exp) {
// 	string strFunc = compileExp(&(exp->expression()));
// 	vector<string> strArgs;
// 	for (auto arg : exp->arguments())
// 		strArgs.push_back(compileExp((&arg)->get()));
// 	string strArg = boost::algorithm::join(strArgs, ", ");
// 	string result = strFunc + "(" + strArg + ")";
// 	// if (debug)
// 	// 	result = "(FunctionCall: " + result + ")";
// 	return result;
// }

// string LlvmCompiler::compileExp(NewExpression const* exp) {
// 	// return "new " + compileTypeName(exp->typeName());
// 	return "new";
// }

// string LlvmCompiler::compileExp(MemberAccess const* exp) {
// 	string strBase = compileExp(&(exp->expression()));
// 	string strMember = exp->memberName();
// 	return strBase + "." + strMember;
// }

// string LlvmCompiler::compileExp(IndexAccess const* exp) {
// 	string strBase = compileExp(&(exp->baseExpression()));
// 	string strIndex = compileExp(exp->indexExpression());
// 	return strBase + "[" + strIndex + "]";
// }

// string LlvmCompiler::compileExp(Identifier const *exp) {
// 	string result = exp->name();
// 	// if (debug)
// 	// 	result = "(Identifier: " + result + ")";
// 	return result;
// }

// string LlvmCompiler::compileExp(ElementaryTypeNameExpression const *exp) {
// 	return exp->typeName().toString();
// }


/********************************************************
 *               Compile Contract
 ********************************************************/

void LlvmCompiler::compileContract(const ContractDefinition* contract) {
	// prepare environment
	GlobalNamedValues.clear();

	// // structs
	// for (const StructDefinition* st: contract->definedStructs())
	// 	result = result + "\n\n" + compileStructDecl(st);

	// // enum
	// // for (const EnumDefinition* en: contract->definedStructs())
	// // 	result = result + "\n\n" + compileStruct(st);

	// make contra
	string contractName = contract->name();
	CompilingModule =
		llvm::make_unique<llvm::Module>(contractName, Context);

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
	vector<llvm::Type*> elements;
	for (auto var : st->members()) {
		elements.push_back(compileTypeName(var->typeName()));
	}
	return llvm::StructType::create(Context, elements, name);
}


Value* LlvmCompiler::compileGlobalVarDecl(const VariableDeclaration* var) {
	llvm::Type* type = compileTypePointer(var->type());
	// Value* initVal = compileExp(var->value().get());
	string name = var->name();

	llvm::GlobalVariable* llvmVar =
		new llvm::GlobalVariable(type, false,
								 llvm::GlobalVariable::CommonLinkage,
								 nullptr, name);
	GlobalNamedValues[name] = llvmVar;
	return llvmVar;
}

Value* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var) {

	llvm::Type* type = compileTypePointer(var.type());
	string name = var.name();

	type->print(llvm::outs(), true);


	LogDebug("compileExp: VariableDeclaration 10");

	auto llvmVar = Builder.CreateAlloca(type, nullptr, name);
	// LocalNamedValues[name] = llvmVar;
	LogDebug("compileExp: VariableDeclaration 11");

	return llvmVar;
}

Value* LlvmCompiler::compileLocalVarDecl(VariableDeclaration& var,
										 const Expression* value) {
	LogDebug("compileExp: VariableDeclaration 1");
	auto llvmVar = compileLocalVarDecl(var);
	LogDebug("compileExp: VariableDeclaration 2");
	auto llvmValue = compileExp(value);
	LogDebug("compileExp: VariableDeclaration 3");
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
		llvmRetType = compileTypePointer(returnTypes.at(0));
	else {
		// TODO: handle returned type tuple
		LogError("CompileFunc: unknown returned function type");
	}
	vector<llvm::Type*> llvmParamTypes;
	auto params = func->parameters();
	for (auto p: params)
		llvmParamTypes.push_back(compileTypePointer(p->type()));
	llvm::FunctionType* llvmFuncType =
		llvm::FunctionType::get(llvmRetType, llvmParamTypes, false);

	// create function
	llvm::Function *llvmFunc =
		llvm::Function::Create(llvmFuncType, llvm::Function::CommonLinkage,
							   funcName, CompilingModule.get());

	// set names for parameters and also record it to local names
	int index = 0;
	for (auto &arg : llvmFunc->args()) {
		string paramName = params.at(index)->name();
		arg.setName(paramName);
		LocalNamedValues[paramName] = &arg;
		index++;
	}

	// translate new function
	llvm::BasicBlock *block =
		llvm::BasicBlock::Create(Context, "entry", llvmFunc);
	Builder.SetInsertPoint(block);

	for (auto stmt: func->body().statements())
		compileStmt(*stmt);

	return llvmFunc;
}

/********************************************************
 *                 Compile Statements
 ********************************************************/

Value* LlvmCompiler::compileStmt(Statement const& stmt) {
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
	if (auto s = dynamic_cast<BreakableStatement const*>(&stmt)) {
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
	return nullptr;
}

Value* LlvmCompiler::compileStmt(InlineAssembly const* stmt) {
	// TODO
	LogError("compileStmt: InlineAssembly: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(Block const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	llvm::BasicBlock* block =
		llvm::BasicBlock::Create(Context, "block", llvmFunc);
	Builder.SetInsertPoint(block);

	for (auto s : stmt->statements())
		compileStmt(*s);

	llvmFunc->getBasicBlockList().push_back(block);
	return nullptr;
}

Value* LlvmCompiler::compileStmt(PlaceholderStatement const* stmt) {
	// TODO
	LogError("compileStmt: PlaceholderStatement: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(IfStatement const* stmt) {
	// 	string strIndent = createIndent(indent);
	// 	string strCond = compileExp(&(stmt->condition()));
	// 	string strTrue = compileStmt(stmt->trueStatement(), indent);
	// 	Statement const* falseStmt = stmt->falseStatement();
	// 	string result =
	// 		strIndent + "if (" + strCond + ")\n" + strTrue;
	// 	if (falseStmt != nullptr) {
	// 		string strFalse = compileStmt(*(falseStmt), indent);
	// 		result = result + strIndent + "else\n" + strFalse;
	// 	}
	// 	return result;

	Value* condValue = compileExp(&(stmt->condition()));
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	llvm::BasicBlock* thenBlock =
		llvm::BasicBlock::Create(Context, "then", llvmFunc);
	llvm::BasicBlock* elseBlock =
		llvm::BasicBlock::Create(Context, "else");
	llvm::BasicBlock* mergeBlock =
		llvm::BasicBlock::Create(Context, "ifmerge");

	Builder.CreateCondBr(condValue, thenBlock, elseBlock);

	Value* thenValue = compileStmt(stmt->trueStatement());
	Builder.SetInsertPoint(thenBlock);

	// Builder.CreateBr(mergeBlock);
	// Value* elseValue = compileStmt(stmt->falseStatement());
	// if (elseValue != nullptr) {

	// }

	// llvm::PHINode* phiNode = Builder.CreatePHI()


	// TODO
	return nullptr;
}

Value* LlvmCompiler::compileStmt(BreakableStatement const* stmt) {
	// TODO
	LogError("compileStmt: BreakableStatement: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(WhileStatement const* stmt) {
	// TODO
	LogError("compileStmt: WhileStatement: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(ForStatement const* stmt) {
	// TODO
	LogError("compileStmt: ForStatement: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(Continue const* stmt) {
	// TODO
	LogError("compileStmt: Continue: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(Break const* stmt) {
	// TODO
	LogError("compileStmt: Break: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(Return const* stmt) {
	return Builder.CreateRet(compileExp(stmt->expression()));
}

Value* LlvmCompiler::compileStmt(Throw const* stmt) {
	// TODO
	LogError("compileStmt: Throw: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(EmitStatement const* stmt) {
	// TODO
	LogError("compileStmt: Emit: unhandled");
	return nullptr;
}

Value* LlvmCompiler::compileStmt(VariableDeclarationStatement const* stmt) {
	auto vars = stmt->declarations();
	string result = "";
	if (vars.size() == 1)
		return compileLocalVarDecl(*(vars.at(0)), stmt->initialValue());
	else
		LogError("compileStmt: VariableDeclarationStatement: more than 1 var");

	return nullptr;
}

Value* LlvmCompiler::compileStmt(ExpressionStatement const* stmt) {
	return compileExp(&(stmt->expression()));
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
	LogError("compileExp: unknown expression");
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

	Token::Value op = exp->getOperator();


	switch (op) {
	case Token::Not:
		return Builder.CreateNot(subExp, "not_tmp");

	case Token::BitNot:
		return Builder.CreateNot(subExp, "bitnot_tmp");

	case Token::Inc: {
		Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateAdd(subExp, one, "inc_tmp");
		return Builder.CreateStore(newExp, subExp);
	}

	case Token::Dec: {
		Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateSub(subExp, one, "dec_tmp");
		return Builder.CreateStore(newExp, subExp);
	}

	case Token::Delete:
		LogError("compileExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("compileExp: UnaryOp: unknown operator");
		return nullptr;
	}
}

Value* LlvmCompiler::compileExp(BinaryOperation const* exp) {
	Value* lhs = compileExp(&(exp->leftExpression()));
	Value* rhs = compileExp(&(exp->rightExpression()));
	if (!lhs || !rhs) return nullptr;

	Token::Value op = exp->getOperator();

	switch (op) {
	// TODO: there might be different type of IR Exps for the same token

	case Token::Comma:
		LogError("compileExp: BinaryOp: need to support Comma Exp");
		return nullptr;

	case Token::Or:
		return Builder.CreateOr(lhs, rhs, "or_tmp");

	case Token::And:
		return Builder.CreateOr(lhs, rhs, "and_tmp");

	case Token::BitOr:
		return Builder.CreateOr(lhs, rhs, "bitor_tmp");

	case Token::BitXor:
		return Builder.CreateXor(lhs, rhs, "bitxor_tmp");

	case Token::BitAnd:
		return Builder.CreateXor(lhs, rhs, "bitand_tmp");

	case Token::SHL:
		return Builder.CreateShl(lhs, rhs, "shl_tmp");

	case Token::SAR:
		LogError("compileExp: BinaryOp: need to support SAR");
		return nullptr;

	case Token::SHR:
		return Builder.CreateLShr(lhs, rhs, "shr_tmp");

	case Token::Add:
		return Builder.CreateAdd(lhs, rhs, "add_tmp");

	case Token::Sub:
		return Builder.CreateSub(lhs, rhs, "sub_tmp");

	case Token::Mul:
		return Builder.CreateMul(lhs, rhs, "mul_tmp");

	case Token::Div:
		return Builder.CreateUDiv(lhs, rhs, "div_tmp");

	case Token::Mod:
		return Builder.CreateURem(lhs, rhs, "rem_tmp");

	case Token::Exp:
		LogError("compileExp: BinaryOp: unhandled Exponential Exp");
		return nullptr;

	default:
		LogError("compileExp: BinaryOp: unknown operator");
		return nullptr;
	}
}

Value* LlvmCompiler::compileExp(FunctionCall const* exp) {
	string funcName = *(exp->names().at(0));
	cout << "FuncCall: FuncName: " << funcName << endl;
	llvm::Function *callee = CompilingModule->getFunction(funcName);

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
	// 	string strBase = compileExp(&(exp->expression()));
	// 	string strMember = exp->memberName();
	// 	return strBase + "." + strMember;
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
		LogError("compileExp: Literal: unknown token");
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
	return compileTypePointer(ty);
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

llvm::Type* LlvmCompiler::compileTypePointer(TypePointer type) {
	if (auto t = dynamic_pointer_cast<IntegerType const>(type)) {
		if (t != nullptr)
			return llvm::IntegerType::get(Context, t->numBits());
	}
	// else if (dynamic_pointer_cast<FixedPointType const>(type) != nullptr)
	// 	result = "int";
	// else if (dynamic_pointer_cast<RationalNumberType const>(type) != nullptr)
	// 	result = "ratio";
	// else if (auto t = dynamic_pointer_cast<StringLiteralType const>(type)) {
	// 	if (t != nullptr)
	// 		return llvm::IntegerType::get(Context, t->numBits());
	// }
	// else if (dynamic_pointer_cast<FixedBytesType const>(type) != nullptr)
	// 	result = "char*";
	else if (dynamic_pointer_cast<BoolType const>(type) != nullptr) {
		if (t != nullptr)
			// consider BoolType as an IntType 8 bit
			return llvm::IntegerType::get(Context, 8);
	}
	// else if (dynamic_pointer_cast<ReferenceType const>(type) != nullptr)
	// 	result = "reference";
	// else if (dynamic_pointer_cast<ContractType const>(type) != nullptr)
	// 	result = "contract";
	// else if (dynamic_pointer_cast<EnumType const>(type) != nullptr)
	// 	result = "enum";
	// else if (dynamic_pointer_cast<TupleType const>(type) != nullptr)
	// 	result = "tuple";
	// else if (dynamic_pointer_cast<FunctionType const>(type) != nullptr)
	// 	result = "function";
	// else if (dynamic_pointer_cast<MappingType const>(type) != nullptr)
	// 	result = "mapping";
	// else if (dynamic_pointer_cast<TypeType const>(type) != nullptr)
	// 	result = "(TypeType)";
	// else if (dynamic_pointer_cast<ModifierType const>(type) != nullptr)
	// 	result = "(ModifierType)";
	// else if (dynamic_pointer_cast<ModuleType const>(type) != nullptr)
	// 	result = "(ModuleType)";
	// else if (dynamic_pointer_cast<MagicType const>(type) != nullptr)
	// 	result = "(MagicType)";
	// else if (dynamic_pointer_cast<InaccessibleDynamicType const>(type) != nullptr)
	// 	result = "(InaccessibleDynamicType)";
	// else result = "(Unknown Type)";

	LogError("compileTypePointer: unhandle type");
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

string LlvmCompiler::stringOf(llvm::BasicBlock* block) {
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
