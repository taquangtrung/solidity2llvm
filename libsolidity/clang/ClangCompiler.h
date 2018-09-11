#pragma once

#include "libsolidity/parsing/Scanner.h"
#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTVisitor.h"
#include "libevmasm/LinkerObject.h"

#include <clang/AST/AST.h>
#include <clang/AST/Decl.h>
#include <clang/Basic/FileSystemOptions.h>
#include <clang/Basic/FileManager.h>


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

	// compile a contract to ClangAST
	clang::Expr* transExp(Expression const* exp);
	clang::Expr* transExp(Conditional const* exp);
	clang::Expr* transExp(Assignment const* exp);
	clang::Expr* transExp(TupleExpression const* exp);
	clang::Expr* transExp(UnaryOperation const* exp);
	clang::Expr* transExp(BinaryOperation const* exp);
	clang::Expr* transExp(FunctionCall const* exp);
	clang::Expr* transExp(NewExpression const* exp);
	clang::Expr* transExp(MemberAccess const* exp);
	clang::Expr* transExp(IndexAccess const* exp);
	clang::Expr* transExp(PrimaryExpression const* exp);
	clang::Expr* transExp(Identifier const* exp);
	clang::Expr* transExp(ElementaryTypeNameExpression const* exp);
	clang::Expr* transExp(Literal const* exp);

	clang::UnaryOperatorKind transUnaryOpcode(Token::Value op);
	clang::BinaryOperatorKind transBinaryOpcode(Token::Value op);
	/* clang::CastKind transCastOpcode(Token::Value op); */

	clang::SourceLocation transLocation(SourceLocation loc);

	// compile a contract to string.
	void compileContract(const ContractDefinition &contract, const bytes &metadata);

	string clangString(const ContractDefinition* contract, StringMap sourceCodes);

	/* string compileStructDecl(const StructDefinition* st); */
	/* string compileVarDecl(const VariableDeclaration* var, int indent); */
	/* string compileVarDecl(VariableDeclaration& var, const Expression* value, int indent); */
	/* string compileFunction(const FunctionDefinition* func); */

	/* string compileStmt(Statement const& stmt, int indent); */
	/* string compileStmt(InlineAssembly const* stmt, int indent); */
	/* string compileStmt(Block const* stmt, int indent); */
	/* string compileStmt(PlaceholderStatement const* stmt, int indent); */
	/* string compileStmt(IfStatement const* stmt, int indent); */
	/* string compileStmt(BreakableStatement const* stmt, int indent); */
	/* string compileStmt(WhileStatement const* stmt, int indent); */
	/* string compileStmt(ForStatement const* stmt, int indent); */
	/* string compileStmt(Continue const* stmt, int indent); */
	/* string compileStmt(Break const* stmt, int indent); */
	/* string compileStmt(Return const* stmt, int indent); */
	/* string compileStmt(Throw const* stmt, int indent); */
	/* string compileStmt(EmitStatement const* stmt, int indent); */
	/* string compileStmt(VariableDeclarationStatement const* stmt, int indent); */
	/* string compileStmt(ExpressionStatement const* stmt, int indent); */

	/* string compileExp(Expression const* exp); */
	/* string compileExp(Conditional const* exp); */
	/* string compileExp(Assignment const* exp); */
	/* string compileExp(TupleExpression const* exp); */
	/* string compileExp(UnaryOperation const* exp); */
	/* string compileExp(BinaryOperation const* exp); */
	/* string compileExp(FunctionCall const* exp); */
	/* string compileExp(NewExpression const* exp); */
	/* string compileExp(MemberAccess const* exp); */
	/* string compileExp(IndexAccess const* exp); */
	/* string compileExp(PrimaryExpression const* exp); */
	/* string compileExp(Identifier const* exp); */
	/* string compileExp(ElementaryTypeNameExpression const* exp); */
	/* string compileExp(Literal const* exp); */

	/* string compileTypeName(ElementaryTypeName const* type); */
	/* string compileTypeName(UserDefinedTypeName const* type); */
	/* string compileTypeName(FunctionTypeName const* type); */
	/* string compileTypeName(Mapping const* type); */
	/* string compileTypeName(ArrayTypeName const* type); */

	/* string compileOperator(Token::Value op); */
	/* string compileTypePointer(TypePointer type); */
	/* string compileTypeName(TypeName const* type); */

	/* string createIndent(int indent); */
	/* void resetGlobalVars(); */

	/* bool isSimpleExp(Expression const* exp); */

private:
	const ContractDefinition* CompilingContract;
	StringMap compilingSourceCodes;
	bool debug;

};

} // end of namespace solidity
} // end of namespace dev
