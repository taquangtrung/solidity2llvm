#pragma once

#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTVisitor.h"
#include "libevmasm/LinkerObject.h"

#include <clang/AST/ASTContext.h>

#include <map>

using namespace std;

namespace dev {

namespace solidity {

class ClangCompiler {

public:
	explicit ClangCompiler():
	debug(false),
	CompilingContract(nullptr)
	{ }

	// Compiles a contract.
	void compileContract(const ContractDefinition &contract, const bytes &metadata);

	string clangString(const ContractDefinition* contract);

	string compileStructDecl(const StructDefinition* st);
	string compileVarDecl(const VariableDeclaration* var, int indent);
	string compileVarDecl(VariableDeclaration& var, const Expression* value, int indent);
	string compileFunction(const FunctionDefinition* func);

	string compileStmt(Statement const& stmt, int indent);
	string compileStmt(InlineAssembly const* stmt, int indent);
	string compileStmt(Block const* stmt, int indent);
	string compileStmt(PlaceholderStatement const* stmt, int indent);
	string compileStmt(IfStatement const* stmt, int indent);
	string compileStmt(BreakableStatement const* stmt, int indent);
	string compileStmt(WhileStatement const* stmt, int indent);
	string compileStmt(ForStatement const* stmt, int indent);
	string compileStmt(Continue const* stmt, int indent);
	string compileStmt(Break const* stmt, int indent);
	string compileStmt(Return const* stmt, int indent);
	string compileStmt(Throw const* stmt, int indent);
	string compileStmt(EmitStatement const* stmt, int indent);
	string compileStmt(VariableDeclarationStatement const* stmt, int indent);
	string compileStmt(ExpressionStatement const* stmt, int indent);

	string compileExp(Expression const* exp);
	string compileExp(Conditional const* exp);
	string compileExp(Assignment const* exp);
	string compileExp(TupleExpression const* exp);
	string compileExp(UnaryOperation const* exp);
	string compileExp(BinaryOperation const* exp);
	string compileExp(FunctionCall const* exp);
	string compileExp(NewExpression const* exp);
	string compileExp(MemberAccess const* exp);
	string compileExp(IndexAccess const* exp);
	string compileExp(PrimaryExpression const* exp);
	string compileExp(Identifier const* exp);
	string compileExp(ElementaryTypeNameExpression const* exp);
	string compileExp(Literal const* exp);

	string compileTypeName(ElementaryTypeName const* type);
	string compileTypeName(UserDefinedTypeName const* type);
	string compileTypeName(FunctionTypeName const* type);
	string compileTypeName(Mapping const* type);
	string compileTypeName(ArrayTypeName const* type);

	string compileOperator(Token::Value op);
	string compileTypePointer(TypePointer type);
	string compileTypeName(TypeName const* type);

	string createIndent(int indent);
	void resetGlobalVars();

	bool isSimpleExp(Expression const* exp);

private:
	const ContractDefinition* CompilingContract;
	bool debug;

};

} // end of namespace solidity
} // end of namespace dev
