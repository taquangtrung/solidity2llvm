#pragma once

#include "libsolidity/parsing/Scanner.h"
#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTVisitor.h"
#include "libevmasm/LinkerObject.h"

#include <llvm/IR/Value.h>

#include <map>

using namespace std;

namespace dev {

namespace solidity {

class LlvmCompiler {

public:
	explicit LlvmCompiler():
	debug(false),
	CompilingContract(nullptr)
	{ }


	llvm::Value* transContract(const ContractDefinition* contract);
	llvm::Value* transFunc(FunctionDefinition const* func);

	llvm::Value* transStmt(Statement const& stmt);
	llvm::Value* transStmt(InlineAssembly const* stmt);
	llvm::Value* transStmt(Block const* stmt);
	llvm::Value* transStmt(PlaceholderStatement const* stmt);
	llvm::Value* transStmt(IfStatement const* stmt);
	llvm::Value* transStmt(BreakableStatement const* stmt);
	llvm::Value* transStmt(WhileStatement const* stmt);
	llvm::Value* transStmt(ForStatement const* stmt);
	llvm::Value* transStmt(Continue const* stmt);
	llvm::Value* transStmt(Break const* stmt);
	llvm::Value* transStmt(Return const* stmt);
	llvm::Value* transStmt(Throw const* stmt);
	llvm::Value* transStmt(EmitStatement const* stmt);
	llvm::Value* transStmt(VariableDeclarationStatement const* stmt);
	llvm::Value* transStmt(ExpressionStatement const* stmt);


	// compile a contract to ClangAST
	llvm::Value* transExp(Expression const* exp);
	llvm::Value* transExp(Conditional const* exp);
	llvm::Value* transExp(Assignment const* exp);
	llvm::Value* transExp(TupleExpression const* exp);
	llvm::Value* transExp(UnaryOperation const* exp);
	llvm::Value* transExp(BinaryOperation const* exp);
	llvm::Value* transExp(FunctionCall const* exp);
	llvm::Value* transExp(NewExpression const* exp);
	llvm::Value* transExp(MemberAccess const* exp);
	llvm::Value* transExp(IndexAccess const* exp);
	llvm::Value* transExp(PrimaryExpression const* exp);
	llvm::Value* transExp(Identifier const* exp);
	llvm::Value* transExp(ElementaryTypeNameExpression const* exp);
	llvm::Value* transExp(Literal const* exp);

	// compile a contract to string.
	void compileContract(const ContractDefinition &contract, const bytes &metadata);

	string llvmString(const ContractDefinition* contract, StringMap sourceCodes);

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
