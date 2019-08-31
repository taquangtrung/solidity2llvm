/*
	This file is not a part of Solidity.
*/
/**
 * @author Ta Quang Trung.
 * @date 2018
 * Debugging
 */

#pragma once

#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTPrinter.h"
#include "libsolidity/ast/ASTVisitor.h"
#include "libsolidity/ast/Types.h"
#include "libsolidity/ast/TypeProvider.h"
#include "libsolidity/interface/Debug.h"
#include "libevmasm/LinkerObject.h"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Bitcode/BitcodeWriter.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

#include "llvm/LinkAllPasses.h"

#include <map>
#include <stack>

using namespace std;

using Value = llvm::Value;
using BasicBlock = llvm::BasicBlock;

namespace dev {

namespace solidity {

typedef struct _LoopInfo {
	BasicBlock* loopHead;
	BasicBlock* loopEnd;
} LoopInfo;

class LlvmCompiler {

 public:
	explicit LlvmCompiler():
	CompilingContract(nullptr), Builder(Context) { }

	void compileContract(const ContractDefinition* contract);

	// compile declarations
	llvm::StructType* compileStructDecl(const StructDefinition* st);
	Value* compileGlobalVarDecl(const VariableDeclaration* var);
	Value* compileLocalVarDecl(VariableDeclaration& var);
	Value* compileLocalVarDecl(VariableDeclaration& var, const Expression* value);
	llvm::Function* compileFunction(FunctionDefinition const* func);

	// compile statements
	void compileStmt(Statement const& stmt);
	void compileStmt(InlineAssembly const* stmt);
	void compileStmt(Block const* stmt);
	void compileStmt(PlaceholderStatement const* stmt);
	void compileStmt(IfStatement const* stmt);
	void compileStmt(WhileStatement const* stmt);
	void compileStmt(ForStatement const* stmt);
	void compileStmt(Continue const* stmt);
	void compileStmt(Break const* stmt);
	void compileStmt(Return const* stmt);
	void compileStmt(Throw const* stmt);
	void compileStmt(EmitStatement const* stmt);
	void compileStmt(VariableDeclarationStatement const* stmt);
	void compileStmt(ExpressionStatement const* stmt);

	// compile expressions
	Value* compileExp(Expression const* exp);
	Value* compileExp(Conditional const* exp);
	Value* compileExp(Assignment const* exp);
	Value* compileExp(TupleExpression const* exp);
	Value* compileExp(UnaryOperation const* exp);
	Value* compileExp(BinaryOperation const* exp);
	Value* compileExp(FunctionCall const* exp);
	Value* compileExp(NewExpression const* exp);
	Value* compileExp(MemberAccess const* exp);
	Value* compileExp(IndexAccess const* exp);
	Value* compileExp(PrimaryExpression const* exp);
	Value* compileExp(Identifier const* exp);
	Value* compileExp(ElementaryTypeNameExpression const* exp);
	Value* compileExp(Literal const* exp);

	// compile types
	llvm::Type* compileTypeName(TypeName const* type);
	llvm::Type* compileTypeName(ElementaryTypeName const* type);
	llvm::Type* compileTypeName(UserDefinedTypeName const* type);
	llvm::Type* compileTypeName(FunctionTypeName const* type);
	llvm::Type* compileTypeName(Mapping const* type);
	llvm::Type* compileTypeName(ArrayTypeName const* type);
	llvm::Type* compileType(TypePointer type);

	// compile a contract to string.
	string llvmString(const ContractDefinition* contract, StringMap sourceCodes);

	// supporting functions
	string stringOf(llvm::Module* module);
	string stringOf(llvm::Function* func);
	string stringOf(llvm::BasicBlock* block);
	string stringOf(llvm::Value* value);
	string stringOf(llvm::Type* typ);
	Value* findNamedValue(string name);

private:
	const ContractDefinition* CompilingContract;
	StringMap compilingSourceCodes;

private:
	llvm::LLVMContext Context;
	llvm::IRBuilder<> Builder;
	std::unique_ptr<llvm::Module> Module;
	std::string ContractName;
	std::map<std::string, Value *> GlobalNamedValues;
	std::map<std::string, Value *> LocalNamedValues;
	std::map<std::string, llvm::StructType *> NamedStructTypes;
	std::unique_ptr<llvm::legacy::FunctionPassManager> FunctionPM;
	std::stack<LoopInfo> LoopStack;

};

} // end of namespace solidity
} // end of namespace dev
