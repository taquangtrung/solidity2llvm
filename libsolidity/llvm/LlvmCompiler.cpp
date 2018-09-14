#include <iostream>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/LlvmCompiler.h"
#include "libsolidity/ast/AST.h"
#include <libsolidity/interface/Exceptions.h>
#include <libdevcore/SHA3.h>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"


using namespace std;
using namespace dev;
using namespace dev::solidity;

static llvm::LLVMContext TheContext;
static llvm::IRBuilder<> Builder(TheContext);
static std::unique_ptr<llvm::Module> TheModule;
static std::map<std::string, llvm::Value *> NamedValues;

/// LogError* - These are little helper functions for error handling.
void LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
}

void LlvmCompiler::compileContract(const ContractDefinition &contract, const bytes &metadata) {
	(void)metadata;
	cout << "RUNNING SEA COMPILER..." << endl;
	auto functions = contract.definedFunctions();
}

/*
   _sourceCodes: maps name of a contract to its source code
 */
string LlvmCompiler::llvmString(const ContractDefinition* contract, StringMap sourceCodes) {
	compilingSourceCodes = sourceCodes;

	transContract(contract);

	return "LLVM IR STRING";

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
 *           Compile declarations to Clang AST
 ********************************************************/

llvm::Value* LlvmCompiler::transContract(const ContractDefinition* contract) {

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

		// functions
		for (const FunctionDefinition* f: contract->definedFunctions())
				transFunc(f);

		return nullptr;
}

llvm::Value* LlvmCompiler::transFunc(const FunctionDefinition* func) {
		return nullptr;
}

/********************************************************
 *           Compile statements to Clang AST
 ********************************************************/

llvm::Value* LlvmCompiler::transStmt(Statement const& stmt) {
	if (auto s = dynamic_cast<InlineAssembly const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<Block const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<PlaceholderStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<IfStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<BreakableStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<Continue const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<Break const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<Return const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<Throw const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<EmitStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<VariableDeclarationStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	if (auto s = dynamic_cast<ExpressionStatement const*>(&stmt)) {
		if (s != nullptr) return transStmt(s);
	}
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(InlineAssembly const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(Block const* stmt) {
	for (auto s : stmt->statements())
		transStmt(*s);
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(PlaceholderStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(IfStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(BreakableStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(WhileStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(ForStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(Continue const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(Break const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(Return const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(Throw const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(EmitStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(VariableDeclarationStatement const* stmt) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transStmt(ExpressionStatement const* stmt) {
	// TODO
	transExp(&(stmt->expression()));
	return nullptr;
}

/********************************************************
 *           Compile expressions to Clang AST
 ********************************************************/

llvm::Value* LlvmCompiler::transExp(Expression const* exp) {
	if (auto e = dynamic_cast<Conditional const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<Assignment const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<TupleExpression const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<UnaryOperation const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<BinaryOperation const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<FunctionCall const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<NewExpression const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<MemberAccess const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<IndexAccess const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	if (auto e = dynamic_cast<PrimaryExpression const*>(exp)) {
		if (e != nullptr) return transExp(e);
	}
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(Conditional const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(Assignment const* exp) {
	llvm::Value* lhs = transExp(&(exp->leftHandSide()));
	llvm::Value* rhs = transExp(&(exp->rightHandSide()));
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(TupleExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(UnaryOperation const* exp) {
	llvm::Value* subExp = transExp(&(exp->subExpression()));
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
		LogError("transExp: UnaryOp: unhandled Delete");
		return nullptr;

	default:
		LogError("transExp: UnaryOp: unknown operator");
		return nullptr;
	}
}

llvm::Value* LlvmCompiler::transExp(BinaryOperation const* exp) {
	llvm::Value* lhs = transExp(&(exp->leftExpression()));
	llvm::Value* rhs = transExp(&(exp->rightExpression()));
	if (!lhs || !rhs) return nullptr;

	Token::Value op = exp->getOperator();

	switch (op) {
	// TODO: there might be different type of IR Exps for the same token

	case Token::Comma:
		LogError("transExp: BinaryOp: need to support Comma Exp");
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
		LogError("transExp: BinaryOp: need to support SAR");
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
		LogError("transExp: BinaryOp: unhandled Exponential Exp");
		return nullptr;

	default:
		LogError("transExp: BinaryOp: unknown operator");
		return nullptr;
	}
}

llvm::Value* LlvmCompiler::transExp(FunctionCall const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(NewExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(MemberAccess const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(IndexAccess const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(PrimaryExpression const* exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(Identifier const *exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(ElementaryTypeNameExpression const *exp) {
	// TODO
	return nullptr;
}

llvm::Value* LlvmCompiler::transExp(Literal const *exp) {
	// TODO
	return nullptr;
}
