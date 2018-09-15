#include <iostream>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/LlvmCompiler.h"
#include "libsolidity/ast/AST.h"
#include <libsolidity/interface/Exceptions.h>
#include <libdevcore/SHA3.h>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"


using namespace std;
using namespace dev;
using namespace dev::solidity;

using BasicBlock = llvm::BasicBlock;

static llvm::LLVMContext CompilingContext;
static llvm::IRBuilder<> Builder(CompilingContext);
static std::unique_ptr<llvm::Module> CompilingModule;
static std::map<std::string, llvm::Value *> NamedValues;

/// LogError* - These are little helper functions for error handling.
void LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
}

/*
   _sourceCodes: maps name of a contract to its source code
 */
string LlvmCompiler::llvmString(const ContractDefinition* contract, StringMap sourceCodes) {
	cout << "start to compile to LLVM IR..." << endl;


	compilingSourceCodes = sourceCodes;

	compileContract(contract);

    string result = "====== OUTPUT LLVM IR ======\n" +
		printLlvmModule(*CompilingModule);

	return result;

	// // update global vars
	// CompilingContract = contract;
	// debug = true;

	// cout << "== OUTPUT C CONTRACT == " << endl;
	// string result = "";

	// // structs
	// for (const StructDefinition* st: contract->definedStructs())
	// 	result = result + "\n\n" + compileStructDecl(st);

	// // enum
	// // for (const EnumDefinition* en: contract->definedStructs())
	// // 	result = result + "\n\n" + compileStruct(st);

	// // state variables
	// for (const VariableDeclaration* var: contract->stateVariables())
	// 	result = result + "\n\n" + compileVarDecl(var, 0);

	// // functions
	// for (const FunctionDefinition* f: contract->definedFunctions())
	// 	result = result + "\n\n" + compileFunction(f);
	// return result;
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

// string LlvmCompiler::compileVarDecl(const VariableDeclaration* var, int indent) {
// 	return createIndent(indent) + compileTypeName(var->typeName())
// 			+ " " + var->name() + ";";
// }

// string LlvmCompiler::compileVarDecl(VariableDeclaration& var, const Expression* value, int indent) {
// 	return createIndent(indent) + compileTypeName(var.typeName())
// 		+ " " + var.name() + " " + compileExp(value) + ";";
// }

// string LlvmCompiler::compileFunction(const FunctionDefinition* func) {
// 	string result;

// 	// returned type
// 	FunctionTypePointer externalType = func->functionType(false);
// 	auto returnTypes = externalType->returnParameterTypes();
// 	string strReturnType = "";
// 	if (returnTypes.size() == 0)
// 		strReturnType = "void";
// 	else if (returnTypes.size() == 1)
// 		strReturnType = compileTypePointer(returnTypes.at(0));
// 	else {
// 		// TODO: handle returned type tuple
// 		strReturnType = "UNK_TUPLE";
// 	}
// 	result = result + strReturnType;

// 	// function name
// 	string name = func->name();
// 	if (func->isConstructor())
// 		name = CompilingContract->name();
// 	result = result + " " + name;

// 	// function parameters
// 	vector<string> strParams;
// 	for (auto p: func->parameters())
// 		strParams.push_back(compileTypePointer(p->type()) + " " + p->name());
// 	string strParam = boost::algorithm::join(strParams, ", ");
// 	result = result + "(" + strParam + ")";

// 	// function body
// 	string strBody = "";
// 	for (auto stmt: func->body().statements())
// 		strBody = strBody + compileStmt(*stmt, 1) + "\n";

// 	result = result + " {\n" + strBody + "}\n";

// 	return result;
// }

// /*****************************************************
//  *                Compile statements
//  *****************************************************/

// string LlvmCompiler::compileStmt(Statement const& stmt, int indent) {
// 	if (auto s = dynamic_cast<InlineAssembly const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<Block const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<PlaceholderStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<IfStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<BreakableStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<Continue const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<Break const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<Return const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<Throw const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<EmitStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<VariableDeclarationStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<ExpressionStatement const*>(&stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	return "(Unknown Statement)";
// }

// string LlvmCompiler::compileStmt(InlineAssembly const* stmt, int indent) {
// 	// TODO
// 	return "(InlineAssembly)\n";
// }

// string LlvmCompiler::compileStmt(Block const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string result = strIndent + "{\n" ;
// 	for (auto s : stmt->statements())
// 		result = result + compileStmt(*s, indent + 1) + "\n";
// 	result = result + strIndent + "}" ;
// 	return result;
// }

// string LlvmCompiler::compileStmt(PlaceholderStatement const* stmt, int indent) {
// 	// TODO
// 	return "(PlaceholderStatement)";
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

// string LlvmCompiler::compileStmt(Continue const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "continue;";
// }

// string LlvmCompiler::compileStmt(Break const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "break;";
// }

// string LlvmCompiler::compileStmt(Return const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "return " + compileExp(stmt->expression()) + ";";
// }

// string LlvmCompiler::compileStmt(Throw const* stmt, int indent) {
// 	// TODO
// 	return "(Unhandled Throw Statement)";
// }

// string LlvmCompiler::compileStmt(EmitStatement const* stmt, int indent) {
// 	// TODO
// 	return "(Unhandled Emit Statement)";
// }

// string LlvmCompiler::compileStmt(VariableDeclarationStatement const* stmt, int indent) {
// 	auto vars = stmt->declarations();
// 	string result = "";
// 	if (vars.size() == 1)
// 		result = compileVarDecl(*(vars.at(0)), stmt->initialValue(), indent);
// 	else
// 		result = "(Unhandled VariableDeclarationStatement)";
// 	return result;
// }

// string LlvmCompiler::compileStmt(ExpressionStatement const* stmt, int indent) {
// 	string result =
// 		createIndent(indent) + compileExp(&(stmt->expression())) + ";";
// 	// if (debug)
// 	// 	result = "(ExpressionStatement: " + result +")";
// 	return result;
// }


// /********************************************************
//  *                 Compile expressions
//  ********************************************************/

// string LlvmCompiler::compileExp(Expression const* exp) {
// 	if (auto e = dynamic_cast<Conditional const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<Assignment const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<TupleExpression const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<UnaryOperation const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<BinaryOperation const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<FunctionCall const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<NewExpression const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<MemberAccess const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<IndexAccess const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<PrimaryExpression const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	return "(Unknown Expression)";
// }

// string LlvmCompiler::compileExp(Conditional const* exp) {
// 	string result = "(Conditional)";
// 	return result;
// }

// string LlvmCompiler::compileExp(Assignment const* exp) {
// 	string lhs = compileExp(&(exp->leftHandSide()));
// 	string op = compileOperator((exp->assignmentOperator()));
// 	string rhs = compileExp(&(exp->rightHandSide()));
// 	return lhs + " " + op + " " + rhs;
// }

// string LlvmCompiler::compileExp(TupleExpression const* exp) {
// 	string result = "(TupleExpression)";
// 	return result;
// }

// string LlvmCompiler::compileExp(UnaryOperation const* exp) {
// 	string strOp = compileOperator(exp->getOperator());
// 	const Expression* subExp = &(exp->subExpression());
// 	string strSubExp = compileExp(subExp);
// 	if (!isSimpleExp(subExp))
// 		strSubExp = "(" + strSubExp + ")";
// 	string result = strOp + strSubExp;
// 	// if (debug)
// 	// 	result = "(UnaryOperation: " + result + ")";
// 	return result;
// }

// string LlvmCompiler::compileExp(BinaryOperation const* exp) {
// 	string strOp = compileOperator(exp->getOperator());
// 	const Expression* leftExp = &(exp->leftExpression());
// 	string strLeftExp = compileExp(leftExp);
// 	if (!isSimpleExp(leftExp))
// 		strLeftExp = "(" + strLeftExp + ")";
// 	const Expression* rightExp = &(exp->rightExpression());
// 	string strRightExp = compileExp(rightExp);
// 	if (!isSimpleExp(rightExp))
// 		strRightExp = "(" + strRightExp + ")";
// 	string result = strLeftExp + " " + strOp + " " + strRightExp;
// 	// if (debug)
// 	// 	result = "(BinaryOperation: " + result + ")";
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

// string LlvmCompiler::compileExp(PrimaryExpression const* exp) {
// 	if (auto e = dynamic_cast<Identifier const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<ElementaryTypeNameExpression const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	if (auto e = dynamic_cast<Literal const*>(exp)) {
// 		if (e != nullptr) return compileExp(e);
// 	}
// 	return "(Unknown PrimaryExpression)";
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

// string LlvmCompiler::compileExp(Literal const *exp) {
// 	string result = exp->value();
// 	Token::Value token = exp->token();
// 	if (token == Token::StringLiteral)
// 		result = "\"" + result + "\"";
// 	// if (debug)
// 	// 	result = "(Literal: " + result + ")";
// 	return result;
// }

// /************************************************************
//  *                      Compile Types
//  ************************************************************/

// string LlvmCompiler::compileTypePointer(TypePointer type) {
// 	string result = "";

// 	if (dynamic_pointer_cast<IntegerType const>(type) != nullptr)
// 		result = "int";
// 	else if (dynamic_pointer_cast<FixedPointType const>(type) != nullptr)
// 		result = "int";
// 	else if (dynamic_pointer_cast<RationalNumberType const>(type) != nullptr)
// 		result = "ratio";
// 	else if (dynamic_pointer_cast<StringLiteralType const>(type) != nullptr)
// 		result = "string";
// 	else if (dynamic_pointer_cast<FixedBytesType const>(type) != nullptr)
// 		result = "char*";
// 	else if (dynamic_pointer_cast<BoolType const>(type) != nullptr)
// 		result = "bool";
// 	else if (dynamic_pointer_cast<ReferenceType const>(type) != nullptr)
// 		result = "reference";
// 	else if (dynamic_pointer_cast<ContractType const>(type) != nullptr)
// 		result = "contract";
// 	else if (dynamic_pointer_cast<EnumType const>(type) != nullptr)
// 		result = "enum";
// 	else if (dynamic_pointer_cast<TupleType const>(type) != nullptr)
// 		result = "tuple";
// 	else if (dynamic_pointer_cast<FunctionType const>(type) != nullptr)
// 		result = "function";
// 	else if (dynamic_pointer_cast<MappingType const>(type) != nullptr)
// 		result = "mapping";
// 	else if (dynamic_pointer_cast<TypeType const>(type) != nullptr)
// 		result = "(TypeType)";
// 	else if (dynamic_pointer_cast<ModifierType const>(type) != nullptr)
// 		result = "(ModifierType)";
// 	else if (dynamic_pointer_cast<ModuleType const>(type) != nullptr)
// 		result = "(ModuleType)";
// 	else if (dynamic_pointer_cast<MagicType const>(type) != nullptr)
// 		result = "(MagicType)";
// 	else if (dynamic_pointer_cast<InaccessibleDynamicType const>(type) != nullptr)
// 		result = "(InaccessibleDynamicType)";
// 	else result = "(Unknown Type)";

// 	return result;
// }

// string LlvmCompiler::compileTypeName(TypeName const* type) {
// 	if (auto t = dynamic_cast<ElementaryTypeName const*>(type)) {
// 		if (t != nullptr) return compileTypeName(t);
// 	}
// 	if (auto t = dynamic_cast<UserDefinedTypeName const*>(type)) {
// 		if (t != nullptr) return compileTypeName(t);
// 	}
// 	if (auto t = dynamic_cast<FunctionTypeName const*>(type)) {
// 		if (t != nullptr) return compileTypeName(t);
// 	}
// 	if (auto t = dynamic_cast<Mapping const*>(type)) {
// 		if (t != nullptr) return compileTypeName(t);
// 	}
// 	if (auto t = dynamic_cast<ArrayTypeName const*>(type)) {
// 		if (t != nullptr) return compileTypeName(t);
// 	}
// 	return "(Unknown Type)";
// }

// string LlvmCompiler::compileTypeName(ElementaryTypeName const* type) {
// 	return (&(type->typeName()))->toString();
// }

// string LlvmCompiler::compileTypeName(UserDefinedTypeName const* type) {
// 	vector<string> namePath = type->namePath();
// 	return boost::algorithm::join(namePath, "_");
// }

// string LlvmCompiler::compileTypeName(FunctionTypeName const* type) {
// 	// TODO
// 	return "(Unhandled FunctionTypeName)";
// }

// string LlvmCompiler::compileTypeName(Mapping const* type) {
// 	// TODO
// 	return "(Unhandled Mapping)";
// }

// string LlvmCompiler::compileTypeName(ArrayTypeName const* type) {
// 	// TODO
// 	return "(Unhandled ArrayTypeName)";
// }

// /************************************************************
//  *                Compile Other Constructs
//  ************************************************************/

// string LlvmCompiler::compileOperator(Token::Value op) {
// 	switch (op) {
// 	// unary operators
// 	case Token::Not: return "!";
// 	case Token::BitNot: return "~";
// 	case Token::Inc: return "++";
// 	case Token::Dec: return "--";
// 	case Token::Delete: return "delete";
// 	// binary op
// 	case Token::Comma: return ",";
// 	case Token::Or: return "||";
// 	case Token::And: return "&&";
// 	case Token::BitOr: return "|";
// 	case Token::BitXor: return "^";
// 	case Token::BitAnd: return "&";
// 	case Token::SHL: return "<<";
// 	case Token::SAR: return ">>";
// 	case Token::SHR: return ">>>";
// 	case Token::Add: return "+";
// 	case Token::Sub: return "-";
// 	case Token::Mul: return "*";
// 	case Token::Div: return "/";
// 	case Token::Mod: return "%";
// 	case Token::Exp: return "**";
// 	// comparison
// 	case Token::Equal: return "==";
// 	case Token::NotEqual: return "!=";
// 	case Token::LessThan: return "<";
// 	case Token::GreaterThan: return ">";
// 	case Token::LessThanOrEqual: return "<=";
// 	case Token::GreaterThanOrEqual: return ">=";
// 	// assigment
// 	case Token::Assign: return "=";
// 	case Token::AssignBitOr: return "|=";
// 	case Token::AssignBitXor: return "^=";
// 	case Token::AssignBitAnd: return "&=";
// 	case Token::AssignShl: return "<<=";
// 	case Token::AssignSar: return ">>=";
// 	case Token::AssignShr: return ">>>=";
// 	case Token::AssignAdd: return "+=";
// 	case Token::AssignSub: return "-=";
// 	case Token::AssignMul: return "*=";
// 	case Token::AssignDiv: return "/=";
// 	case Token::AssignMod: return "%=";
// 	// default
// 	default: return "(Unknown Operator)";
// 	}
// }


// /*************************************************************
//  *                      Utility Functions
//  *************************************************************/

// string LlvmCompiler::createIndent(int indent) {
// 	string result = "";
// 	for (int i = 0; i < indent; i++)
// 		result = result + "  ";
// 	return result;
// }

// void LlvmCompiler::resetGlobalVars() {
// }

// bool LlvmCompiler::isSimpleExp(Expression const* exp) {
// 	if (dynamic_cast<PrimaryExpression const*>(exp) != nullptr)
// 		return true;
// 	if (dynamic_cast<MemberAccess const*>(exp) != nullptr)
// 		return true;
// 	if (dynamic_cast<IndexAccess const*>(exp) != nullptr)
// 		return true;
// 	return false;
// }


/********************************************************
 *               Compile Declarations
 ********************************************************/

void LlvmCompiler::compileContract(const ContractDefinition* contract) {

	// // update global vars
	// CompilingContract = contract;
	// debug = true;

	// cout << "== OUTPUT C CONTRACT == " << endl;
	// string result = "";

	// // structs
	// for (const StructDefinition* st: contract->definedStructs())
	// 	result = result + "\n\n" + compileStructDecl(st);

	// // enum
	// // for (const EnumDefinition* en: contract->definedStructs())
	// // 	result = result + "\n\n" + compileStruct(st);

	// // state variables
	// for (const VariableDeclaration* var: contract->stateVariables())
	// 	result = result + "\n\n" + compileVarDecl(var, 0);

	CompilingModule =
		llvm::make_unique<llvm::Module>("smartcontract", CompilingContext);


	// functions
	for (const FunctionDefinition* f: contract->definedFunctions())
		compileFunc(f);
}

llvm::Function* LlvmCompiler::compileFunc(const FunctionDefinition* func) {
	FunctionTypePointer funcType = func->functionType(false);

	// function name
	string funcName = func->name();

	// compile returned type
	auto returnTypes = funcType->returnParameterTypes();
	llvm::Type* llvmRetType;
	if (returnTypes.size() == 0)
		llvmRetType = llvm::Type::getVoidTy(CompilingContext);
	else if (returnTypes.size() == 1)
		llvmRetType = compileTypePointer(returnTypes.at(0));
	else {
		// TODO: handle returned type tuple
		LogError("CompileFunc: unknown returned function type");
	}


	// compile parameters' types
	vector<llvm::Type*> llvmParamTypes;
	for (auto p: func->parameters())
		llvmParamTypes.push_back(compileTypePointer(p->type()));

	llvm::FunctionType* llvmFuncType =
		llvm::FunctionType::get(llvmRetType, llvmParamTypes, false);

	llvm::Function *llvmFunc =
		llvm::Function::Create(llvmFuncType, llvm::Function::CommonLinkage,
							   funcName, CompilingModule.get());

	// // already translated
	// if (!llvmFunc)
	// 	return llvmFunc;

	// translate new function
	BasicBlock *BB = BasicBlock::Create(CompilingContext, "entry", llvmFunc);
	Builder.SetInsertPoint(BB);

	llvm::Value* llvmStmt = nullptr;
	for (auto s: func->body().statements())
		llvmStmt = compileStmt(*s);

	if (llvmStmt != nullptr) {
		Builder.CreateRet(llvmStmt);
		return llvmFunc;
	}

	return nullptr;
}

/********************************************************
 *                 Compile Statements
 ********************************************************/

llvm::Value* LlvmCompiler::compileStmt(Statement const& stmt) {
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

llvm::Value* LlvmCompiler::compileStmt(InlineAssembly const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(Block const* stmt) {
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* block = BasicBlock::Create(CompilingContext, "block", llvmFunc);
	Builder.SetInsertPoint(block);

	for (auto s : stmt->statements())
		compileStmt(*s);

	llvmFunc->getBasicBlockList().push_back(block);
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(PlaceholderStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(IfStatement const* stmt) {
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

	llvm::Value* condValue = compileExp(&(stmt->condition()));
	llvm::Function* llvmFunc = Builder.GetInsertBlock()->getParent();

	BasicBlock* thenBlock = BasicBlock::Create(CompilingContext, "then", llvmFunc);
	BasicBlock* elseBlock = BasicBlock::Create(CompilingContext, "else");
	BasicBlock* mergeBlock = BasicBlock::Create(CompilingContext, "ifmerge");

	Builder.CreateCondBr(condValue, thenBlock, elseBlock);

	llvm::Value* thenValue = compileStmt(stmt->trueStatement());
	Builder.SetInsertPoint(thenBlock);

	// Builder.CreateBr(mergeBlock);
	// llvm::Value* elseValue = compileStmt(stmt->falseStatement());
	// if (elseValue != nullptr) {

	// }

	// llvm::PHINode* phiNode = Builder.CreatePHI()


	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(BreakableStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(WhileStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(ForStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(Continue const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(Break const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(Return const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(Throw const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(EmitStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(VariableDeclarationStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileStmt(ExpressionStatement const* stmt) {
	// TODO
	compileExp(&(stmt->expression()));
	return nullptr;
}

/********************************************************
 *                Compile Expressions
 ********************************************************/

llvm::Value* LlvmCompiler::compileExp(Expression const* exp) {
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
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(Conditional const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(Assignment const* exp) {
	llvm::Value* lhs = compileExp(&(exp->leftHandSide()));
	llvm::Value* rhs = compileExp(&(exp->rightHandSide()));
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(TupleExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(UnaryOperation const* exp) {
	llvm::Value* subExp = compileExp(&(exp->subExpression()));
	if (!subExp) return nullptr;

	Token::Value op = exp->getOperator();


	switch (op) {
	case Token::Not:
		return Builder.CreateNot(subExp, "Not");

	case Token::BitNot:
		return Builder.CreateNot(subExp, "BitNot");

	case Token::Inc: {
		llvm::Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateAdd(subExp, one, "Inc");
		auto storeExp = Builder.CreateStore(newExp, subExp);
		return storeExp;
	}

	case Token::Dec: {
		llvm::Value* one = llvm::ConstantInt::get(subExp->getType(), 1);
		auto newExp = Builder.CreateSub(subExp, one, "Dec");
		auto storeExp = Builder.CreateStore(newExp, subExp);
		return storeExp;
	}

	case Token::Delete:
		LogError("compileExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("compileExp: UnaryOp: unknown operator");
		return nullptr;
	}
}

llvm::Value* LlvmCompiler::compileExp(BinaryOperation const* exp) {
	llvm::Value* lhs = compileExp(&(exp->leftExpression()));
	llvm::Value* rhs = compileExp(&(exp->rightExpression()));
	if (!lhs || !rhs) return nullptr;

	Token::Value op = exp->getOperator();

	switch (op) {
	// TODO: there might be different type of IR Exps for the same token

	case Token::Comma:
		LogError("compileExp: BinaryOp: need to support Comma Exp");
		return nullptr;

	case Token::Or:
		return Builder.CreateOr(lhs, rhs, "Or");

	case Token::And:
		return Builder.CreateOr(lhs, rhs, "And");

	case Token::BitOr:
		return Builder.CreateOr(lhs, rhs, "BitOr");

	case Token::BitXor:
		return Builder.CreateXor(lhs, rhs, "BitXor");

	case Token::BitAnd:
		return Builder.CreateXor(lhs, rhs, "BitAnd");

	case Token::SHL:
		return Builder.CreateShl(lhs, rhs, "Shl");

	case Token::SAR:
		LogError("compileExp: BinaryOp: need to support SAR");
		return nullptr;

	case Token::SHR:
		return Builder.CreateLShr(lhs, rhs, "Shr");

	case Token::Add:
		return Builder.CreateAdd(lhs, rhs, "Add");

	case Token::Sub:
		return Builder.CreateSub(lhs, rhs, "Sub");

	case Token::Mul:
		return Builder.CreateMul(lhs, rhs, "Mul");

	case Token::Div:
		return Builder.CreateUDiv(lhs, rhs, "Div");

	case Token::Mod:
		return Builder.CreateURem(lhs, rhs, "Rem");

	case Token::Exp:
		LogError("compileExp: BinaryOp: unhandled Exponential Exp");
		return nullptr;

	default:
		LogError("compileExp: BinaryOp: unknown operator");
		return nullptr;
	}
}

llvm::Value* LlvmCompiler::compileExp(FunctionCall const* exp) {
	string funcName = *(exp->names().at(0));
	cout << "FuncCall: FuncName: " << funcName << endl;
	llvm::Function *callee = CompilingModule->getFunction(funcName);

	vector<llvm::Value*> arguments;
	for (auto arg : exp->arguments())
		arguments.push_back(compileExp((&arg)->get()));

	if (callee->arg_size() != arguments.size()) {
		LogError("compileExp: FunctionCall: mistmatch arguments");
		return nullptr;
	}

	return Builder.CreateCall(callee, arguments, "functioncall");
}

llvm::Value* LlvmCompiler::compileExp(NewExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(MemberAccess const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(IndexAccess const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(PrimaryExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(Identifier const *exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(ElementaryTypeNameExpression const *exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::compileExp(Literal const *exp) {
	// TODO
	return nullptr;
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
	// TODO
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(UserDefinedTypeName const* type) {
	// TODO
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(FunctionTypeName const* type) {
	// TODO
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(Mapping const* type) {
	// TODO
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypeName(ArrayTypeName const* type) {
	// TODO
	return nullptr;
}

llvm::Type* LlvmCompiler::compileTypePointer(TypePointer type) {
	if (auto intType = dynamic_pointer_cast<IntegerType const>(type)) {
		if (intType != nullptr)
			return llvm::IntegerType::get(CompilingContext, intType->numBits());
	}
	// else if (dynamic_pointer_cast<FixedPointType const>(type) != nullptr)
	// 	result = "int";
	// else if (dynamic_pointer_cast<RationalNumberType const>(type) != nullptr)
	// 	result = "ratio";
	// else if (dynamic_pointer_cast<StringLiteralType const>(type) != nullptr)
	// 	result = "string";
	// else if (dynamic_pointer_cast<FixedBytesType const>(type) != nullptr)
	// 	result = "char*";
	// else if (dynamic_pointer_cast<BoolType const>(type) != nullptr)
	// 	result = "bool";
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

	return nullptr;
}


/********************************************************
 *                Supporting Functions
 ********************************************************/

string LlvmCompiler::printLlvmModule(llvm::Module& module) {
	string result = "";
	for (auto func = module.begin(); func != module.end(); ++func)
		result = result + "\n**************\n\n" + printLlvmFunc(*func);
	return result;
}

string LlvmCompiler::printLlvmFunc(llvm::Function& func) {
	string result = "";
	for (auto block = func.begin(); block != func.end(); ++block) {
		result = result + printLlvmBlock(*block) + "\n\n";
	}
	return result;
}

string LlvmCompiler::printLlvmBlock(llvm::BasicBlock& block) {
	string result = "";
	for (auto inst = block.begin(); inst != block.end(); ++inst) {
		string strInst;
		llvm::raw_string_ostream rso(strInst);
		inst->print(rso);
		result = result + strInst + "\n";
	}
	return result;
}
