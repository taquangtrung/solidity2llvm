#include <iostream>
#include <boost/algorithm/string/join.hpp>

#include "libsolidity/llvm/ClangCompiler.h"
#include "libsolidity/ast/AST.h"
#include <libsolidity/interface/Exceptions.h>
#include <libdevcore/SHA3.h>
#include <clang/Basic/FileManager.h>


using namespace std;
using namespace dev;
using namespace dev::solidity;

void ClangCompiler::compileContract(const ContractDefinition &contract, const bytes &metadata) {
	(void)metadata;
	cout << "RUNNING SEA COMPILER..." << endl;
	auto functions = contract.definedFunctions();
}

/*
   _sourceCodes: maps name of a contract to its source code
 */
string ClangCompiler::clangString(const ContractDefinition* contract, StringMap sourceCodes) {
	compilingSourceCodes = sourceCodes;

	transContract(contract);

	return "CLANG STRING";

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

// string ClangCompiler::compileStructDecl(const StructDefinition* st) {
// 	string result = "struct " + st->name() + " {\n";
// 	string strIndent = createIndent(1);
// 	for (auto var : st->members())
// 		result = result + strIndent + compileTypeName(var->typeName())
// 			+ " " + var->name() + ";\n";
// 	result = result + "}";
// 	return result;
// }

// string ClangCompiler::compileVarDecl(const VariableDeclaration* var, int indent) {
// 	return createIndent(indent) + compileTypeName(var->typeName())
// 			+ " " + var->name() + ";";
// }

// string ClangCompiler::compileVarDecl(VariableDeclaration& var, const Expression* value, int indent) {
// 	return createIndent(indent) + compileTypeName(var.typeName())
// 		+ " " + var.name() + " " + compileExp(value) + ";";
// }

// string ClangCompiler::compileFunction(const FunctionDefinition* func) {
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

// string ClangCompiler::compileStmt(Statement const& stmt, int indent) {
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

// string ClangCompiler::compileStmt(InlineAssembly const* stmt, int indent) {
// 	// TODO
// 	return "(InlineAssembly)\n";
// }

// string ClangCompiler::compileStmt(Block const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string result = strIndent + "{\n" ;
// 	for (auto s : stmt->statements())
// 		result = result + compileStmt(*s, indent + 1) + "\n";
// 	result = result + strIndent + "}" ;
// 	return result;
// }

// string ClangCompiler::compileStmt(PlaceholderStatement const* stmt, int indent) {
// 	// TODO
// 	return "(PlaceholderStatement)";
// }

// string ClangCompiler::compileStmt(IfStatement const* stmt, int indent) {
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

// string ClangCompiler::compileStmt(BreakableStatement const* stmt, int indent) {
// 	if (auto s = dynamic_cast<WhileStatement const*>(stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	if (auto s = dynamic_cast<ForStatement const*>(stmt)) {
// 		if (s != nullptr) return compileStmt(s, indent);
// 	}
// 	return "(Unknown BreakableStatement)";
// }

// string ClangCompiler::compileStmt(WhileStatement const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string strCond = compileExp(&(stmt->condition()));
// 	string strBody = compileStmt(stmt->body(), indent);
// 	string result = strIndent + "while (" + strCond + ")\n" + strBody;
// 	return result;
// }

// string ClangCompiler::compileStmt(ForStatement const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	string strInit = compileStmt(*(stmt->initializationExpression()), 0);
// 	string strLoop = compileStmt(stmt->loopExpression(), 0);
// 	string strCond = compileExp(stmt->condition());
// 	string strBody = compileStmt(stmt->body(), indent);
// 	string result = strIndent + "for (" + strInit + "; " + strCond +
// 		"; " + strLoop + ")\n" + strBody;
// 	return result;
// }

// string ClangCompiler::compileStmt(Continue const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "continue;";
// }

// string ClangCompiler::compileStmt(Break const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "break;";
// }

// string ClangCompiler::compileStmt(Return const* stmt, int indent) {
// 	string strIndent = createIndent(indent);
// 	return strIndent + "return " + compileExp(stmt->expression()) + ";";
// }

// string ClangCompiler::compileStmt(Throw const* stmt, int indent) {
// 	// TODO
// 	return "(Unhandled Throw Statement)";
// }

// string ClangCompiler::compileStmt(EmitStatement const* stmt, int indent) {
// 	// TODO
// 	return "(Unhandled Emit Statement)";
// }

// string ClangCompiler::compileStmt(VariableDeclarationStatement const* stmt, int indent) {
// 	auto vars = stmt->declarations();
// 	string result = "";
// 	if (vars.size() == 1)
// 		result = compileVarDecl(*(vars.at(0)), stmt->initialValue(), indent);
// 	else
// 		result = "(Unhandled VariableDeclarationStatement)";
// 	return result;
// }

// string ClangCompiler::compileStmt(ExpressionStatement const* stmt, int indent) {
// 	string result =
// 		createIndent(indent) + compileExp(&(stmt->expression())) + ";";
// 	// if (debug)
// 	// 	result = "(ExpressionStatement: " + result +")";
// 	return result;
// }


// /********************************************************
//  *                 Compile expressions
//  ********************************************************/

// string ClangCompiler::compileExp(Expression const* exp) {
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

// string ClangCompiler::compileExp(Conditional const* exp) {
// 	string result = "(Conditional)";
// 	return result;
// }

// string ClangCompiler::compileExp(Assignment const* exp) {
// 	string lhs = compileExp(&(exp->leftHandSide()));
// 	string op = compileOperator((exp->assignmentOperator()));
// 	string rhs = compileExp(&(exp->rightHandSide()));
// 	return lhs + " " + op + " " + rhs;
// }

// string ClangCompiler::compileExp(TupleExpression const* exp) {
// 	string result = "(TupleExpression)";
// 	return result;
// }

// string ClangCompiler::compileExp(UnaryOperation const* exp) {
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

// string ClangCompiler::compileExp(BinaryOperation const* exp) {
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

// string ClangCompiler::compileExp(FunctionCall const* exp) {
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

// string ClangCompiler::compileExp(NewExpression const* exp) {
// 	// return "new " + compileTypeName(exp->typeName());
// 	return "new";
// }

// string ClangCompiler::compileExp(MemberAccess const* exp) {
// 	string strBase = compileExp(&(exp->expression()));
// 	string strMember = exp->memberName();
// 	return strBase + "." + strMember;
// }

// string ClangCompiler::compileExp(IndexAccess const* exp) {
// 	string strBase = compileExp(&(exp->baseExpression()));
// 	string strIndex = compileExp(exp->indexExpression());
// 	return strBase + "[" + strIndex + "]";
// }

// string ClangCompiler::compileExp(PrimaryExpression const* exp) {
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

// string ClangCompiler::compileExp(Identifier const *exp) {
// 	string result = exp->name();
// 	// if (debug)
// 	// 	result = "(Identifier: " + result + ")";
// 	return result;
// }

// string ClangCompiler::compileExp(ElementaryTypeNameExpression const *exp) {
// 	return exp->typeName().toString();
// }

// string ClangCompiler::compileExp(Literal const *exp) {
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

// string ClangCompiler::compileTypePointer(TypePointer type) {
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

// string ClangCompiler::compileTypeName(TypeName const* type) {
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

// string ClangCompiler::compileTypeName(ElementaryTypeName const* type) {
// 	return (&(type->typeName()))->toString();
// }

// string ClangCompiler::compileTypeName(UserDefinedTypeName const* type) {
// 	vector<string> namePath = type->namePath();
// 	return boost::algorithm::join(namePath, "_");
// }

// string ClangCompiler::compileTypeName(FunctionTypeName const* type) {
// 	// TODO
// 	return "(Unhandled FunctionTypeName)";
// }

// string ClangCompiler::compileTypeName(Mapping const* type) {
// 	// TODO
// 	return "(Unhandled Mapping)";
// }

// string ClangCompiler::compileTypeName(ArrayTypeName const* type) {
// 	// TODO
// 	return "(Unhandled ArrayTypeName)";
// }

// /************************************************************
//  *                Compile Other Constructs
//  ************************************************************/

// string ClangCompiler::compileOperator(Token::Value op) {
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

// string ClangCompiler::createIndent(int indent) {
// 	string result = "";
// 	for (int i = 0; i < indent; i++)
// 		result = result + "  ";
// 	return result;
// }

// void ClangCompiler::resetGlobalVars() {
// }

// bool ClangCompiler::isSimpleExp(Expression const* exp) {
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

string ClangCompiler::transContract(const ContractDefinition* contract) {

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
				transFunction(f);

		return "";
}

clang::VarDecl* ClangCompiler::transVarDecl(const VariableDeclaration* var) {
		return nullptr;
}

clang::FunctionDecl* ClangCompiler::transFunction(const FunctionDefinition* func) {
	string result;

	// returned type

	// function name

	// function parameters

	// function body
	string strBody = "";
	for (auto stmt: func->body().statements())
			transStmt(*stmt);

	return nullptr;
}


/********************************************************
 *           Compile statements to Clang AST
 ********************************************************/

clang::Stmt* ClangCompiler::transStmt(Statement const& stmt) {
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

clang::Stmt* ClangCompiler::transStmt(InlineAssembly const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(Block const* stmt) {
	for (auto s : stmt->statements())
		transStmt(*s);
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(PlaceholderStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(IfStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(BreakableStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(WhileStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(ForStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(Continue const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(Break const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(Return const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(Throw const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(EmitStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(VariableDeclarationStatement const* stmt) {
	// TODO
	return nullptr;
}

clang::Stmt* ClangCompiler::transStmt(ExpressionStatement const* stmt) {
	// TODO
	transExp(&(stmt->expression()));
	return nullptr;
}

/********************************************************
 *           Compile expressions to Clang AST
 ********************************************************/

clang::Expr* ClangCompiler::transExp(Expression const* exp) {
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

clang::Expr* ClangCompiler::transExp(Conditional const* exp) {
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(Assignment const* exp) {
	clang::Expr* lhs = transExp(&(exp->leftHandSide()));
	clang::Expr* rhs = transExp(&(exp->rightHandSide()));
	clang::BinaryOperatorKind op =
		transBinaryOpcode(exp->assignmentOperator());
	clang::SourceLocation loc = transLocation(exp->location());
	// string op = compileOperator((exp->assignmentOperator()));

	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(TupleExpression const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(UnaryOperation const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(BinaryOperation const* exp) {
	clang::BinaryOperator clangExp(struct EmptyShell());
	clang::Expr* lhs = transExp(&(exp->leftExpression()));
	clang::Expr* rhs = transExp(&(exp->rightExpression()));
	clang::BinaryOperatorKind op =
		transBinaryOpcode(exp->getOperator());
	clang::SourceLocation loc = transLocation(exp->location());
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(FunctionCall const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(NewExpression const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(MemberAccess const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(IndexAccess const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(PrimaryExpression const* exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(Identifier const *exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(ElementaryTypeNameExpression const *exp) {
	// TODO
	return nullptr;
}

clang::Expr* ClangCompiler::transExp(Literal const *exp) {
	// TODO
	return nullptr;
}


clang::UnaryOperatorKind ClangCompiler::transUnaryOpcode(Token::Value op) {
	switch (op) {
	// unary operators
	case Token::Not: return clang::UO_Not;
	case Token::BitNot: return clang::UO_LNot;
	case Token::Inc: return clang::UO_PostInc;
	case Token::Dec: return clang::UO_PostDec;
	// case Token::Delete: return "delete"; // TODO
	//default: return "(Unknown Operator)";
	}
}

clang::BinaryOperatorKind ClangCompiler::transBinaryOpcode(Token::Value op) {
	switch (op) {
	// binary op
	case Token::Comma: return clang::BO_Comma;
	case Token::Or: return clang::BO_LOr;
	case Token::And: return clang::BO_LAnd;
	case Token::BitOr: return clang::BO_Or;
	case Token::BitXor: return clang::BO_Xor;
	case Token::BitAnd: return clang::BO_And;
	case Token::SHL: return clang::BO_Shl;
	// case Token::SAR: return ">>"; // TODO
	case Token::SHR: return clang::BO_Shr;
	case Token::Add: return clang::BO_Add;
	case Token::Sub: return clang::BO_Sub;
	case Token::Mul: return clang::BO_Mul;
	case Token::Div: return clang::BO_Div;
	case Token::Mod: return clang::BO_Rem;
	// case Token::Exp: return "**"; // TODO
	// comparison
	case Token::Equal: return clang::BO_EQ;
	case Token::NotEqual: return clang::BO_NE;
	case Token::LessThan: return clang::BO_LT;
	case Token::GreaterThan: return clang::BO_GT;
	case Token::LessThanOrEqual: return clang::BO_LE;
	case Token::GreaterThanOrEqual: return clang::BO_GE;
	// assigment
	case Token::Assign: return clang::BO_Assign;
	case Token::AssignBitOr: return clang::BO_OrAssign;
	case Token::AssignBitXor: return clang::BO_XorAssign;
	case Token::AssignBitAnd: return clang::BO_AndAssign;
	case Token::AssignShl: return clang::BO_ShlAssign;
	// case Token::AssignSar: return ">>=";
	case Token::AssignShr: return clang::BO_ShrAssign;
	case Token::AssignAdd: return clang::BO_AddAssign;
	case Token::AssignSub: return clang::BO_SubAssign;
	case Token::AssignMul: return clang::BO_MulAssign;
	case Token::AssignDiv: return clang::BO_DivAssign;
	case Token::AssignMod: return clang::BO_RemAssign;
	//default: return "(Unknown Operator)";
	}
}

// clang::CastKind ClangCompiler::transCastOpcode(Token::Value op) {
// 	switch (op) {
// 	//default: return "(Unknown Operator)";
// 	}
// }


clang::SourceLocation ClangCompiler::transLocation(SourceLocation loc) {
	int line;
	int column;
	string sourceContent = "";
	string fileName = *(loc.sourceName);
	for (auto source: compilingSourceCodes)
		if (source.first == fileName) {
			sourceContent = source.second;
			break;
		}

	// cout << "SOURCE CODE: " << endl << sourceContent << endl;

	CharStream stream = CharStream(sourceContent);
	tie(line, column) = stream.translatePositionToLineColumn(loc.start);
	// cout << "LOC start: " << loc.start << endl;
	// cout << "LOC line: " << line << ", column: " << column << endl;
	// cout << "===" << endl;

	// cout << "LOC end: " << loc.end << endl;
	// tie(line, column) = stream.translatePositionToLineColumn(loc.end);
	// cout << "LOC line: " << line << ", column: " << column << endl;
	// clang::FileSystemOptions fileSystemOptions;
	// clang::FileManager fileManager(fileSystemOptions);
	// // auto fileEntry = fileManager.getFile(fileName);

	clang::CompilerInstance compilerInstance;
	compilerInstance.createFileManager();
	clang::SourceManager& source = compilerInstance.getSourceManager();
	auto& fileManager = compilerInstance.getFileManager();
	const clang::FileEntry* file = fileManager.getFile(fileName);
	return source.translateFileLineCol(file, line, column);
}
