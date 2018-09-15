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


	void compileContract(const ContractDefinition* contract);
	llvm::Function* compileFunc(FunctionDefinition const* func);

	llvm::Value* compileStmt(Statement const& stmt);
	llvm::Value* compileStmt(InlineAssembly const* stmt);
	llvm::Value* compileStmt(Block const* stmt);
	llvm::Value* compileStmt(PlaceholderStatement const* stmt);
	llvm::Value* compileStmt(IfStatement const* stmt);
	llvm::Value* compileStmt(BreakableStatement const* stmt);
	llvm::Value* compileStmt(WhileStatement const* stmt);
	llvm::Value* compileStmt(ForStatement const* stmt);
	llvm::Value* compileStmt(Continue const* stmt);
	llvm::Value* compileStmt(Break const* stmt);
	llvm::Value* compileStmt(Return const* stmt);
	llvm::Value* compileStmt(Throw const* stmt);
	llvm::Value* compileStmt(EmitStatement const* stmt);
	llvm::Value* compileStmt(VariableDeclarationStatement const* stmt);
	llvm::Value* compileStmt(ExpressionStatement const* stmt);

	// compile a contract to ClangAST
	llvm::Value* compileExp(Expression const* exp);
	llvm::Value* compileExp(Conditional const* exp);
	llvm::Value* compileExp(Assignment const* exp);
	llvm::Value* compileExp(TupleExpression const* exp);
	llvm::Value* compileExp(UnaryOperation const* exp);
	llvm::Value* compileExp(BinaryOperation const* exp);
	llvm::Value* compileExp(FunctionCall const* exp);
	llvm::Value* compileExp(NewExpression const* exp);
	llvm::Value* compileExp(MemberAccess const* exp);
	llvm::Value* compileExp(IndexAccess const* exp);
	llvm::Value* compileExp(PrimaryExpression const* exp);
	llvm::Value* compileExp(Identifier const* exp);
	llvm::Value* compileExp(ElementaryTypeNameExpression const* exp);
	llvm::Value* compileExp(Literal const* exp);

	// compile types
	llvm::Type* compileTypeName(TypeName const* type);
	llvm::Type* compileTypeName(ElementaryTypeName const* type);
	llvm::Type* compileTypeName(UserDefinedTypeName const* type);
	llvm::Type* compileTypeName(FunctionTypeName const* type);
	llvm::Type* compileTypeName(Mapping const* type);
	llvm::Type* compileTypeName(ArrayTypeName const* type);
	llvm::Type* compileTypePointer(TypePointer type);

	// compile a contract to string.
	string llvmString(const ContractDefinition* contract, StringMap sourceCodes);

	string printLlvmModule(llvm::Module& module);
	string printLlvmFunc(llvm::Function& func);
	string printLlvmBlock(llvm::BasicBlock& block);

private:
	const ContractDefinition* CompilingContract;
	StringMap compilingSourceCodes;
	bool debug;

};

} // end of namespace solidity
} // end of namespace dev
