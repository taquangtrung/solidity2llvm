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
#include "llvm/Support/Casting.h"

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

#include "llvm/LinkAllPasses.h"

#include <map>
#include <stack>

using namespace std;

using LlContext = llvm::LLVMContext;
using LlModule = llvm::Module;
using LlFunction = llvm::Function;
using LlBlock = llvm::BasicBlock;
using LlValue = llvm::Value;
using LlType = llvm::Type;
using LlStructType = llvm::StructType;
using LlFunctionType = llvm::FunctionType;
using LlGlobalVar = llvm::GlobalVariable;

namespace dev {

namespace solidity {

typedef struct _LoopInfo {
	LlBlock* loopHead;
	LlBlock* loopEnd;
} LoopInfo;

class LlvmCompiler {

 public:
	explicit LlvmCompiler():
	CompilingContract(nullptr), Builder(Context) { }

	void compileContract(const ContractDefinition* contract);

	// compile declarations
	LlStructType* compileStructDecl(const StructDefinition*);
	LlValue* compileGlobalVarDecl(const VariableDeclaration*);
	LlValue* compileLocalVarDecl(VariableDeclaration&);
	LlValue* compileLocalVarDecl(VariableDeclaration&, const Expression*);
	LlFunction* compileFunction(FunctionDefinition const*);

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
	LlValue* compileExp(Expression const*);
	LlValue* compileExp(Conditional const*);
	LlValue* compileExp(Assignment const*);
	LlValue* compileExp(TupleExpression const*);
	LlValue* compileExp(UnaryOperation const*);
	LlValue* compileExp(BinaryOperation const*);
	LlValue* compileExp(FunctionCall const*);
	LlValue* compileExp(NewExpression const*);
	LlValue* compileExp(MemberAccess const*);
	LlValue* compileExp(IndexAccess const*);
	LlValue* compileExp(PrimaryExpression const*);
	LlValue* compileExp(Identifier const*);
	LlValue* compileExp(ElementaryTypeNameExpression const*);
	LlValue* compileExp(Literal const*);

	// compile types
	LlType* compileTypeName(TypeName const*);
	LlType* compileTypeName(ElementaryTypeName const*);
	LlType* compileTypeName(UserDefinedTypeName const*);
	LlType* compileTypeName(FunctionTypeName const*);
	LlType* compileTypeName(Mapping const*);
	LlType* compileTypeName(ArrayTypeName const*);
	LlType* compileType(TypePointer);

	// compile a contract to string.
	string llvmString(const ContractDefinition* contract, StringMap sourceCodes);

	// supporting functions
	string stringOf(LlModule*);
	string stringOf(LlFunction*);
	string stringOf(LlBlock*);
	string stringOf(LlValue*);
	string stringOf(LlType*);
	LlValue* findNamedValue(string);

private:
	const ContractDefinition* CompilingContract;
	StringMap compilingSourceCodes;

private:
	LlContext Context;
	llvm::IRBuilder<> Builder;
	std::unique_ptr<LlModule> Module;
	std::string ContractName;
	std::map<std::string, LlValue *> GlobalNamedValues;
	std::map<std::string, LlValue *> LocalNamedValues;
	std::map<std::string, LlStructType *> NamedStructTypes;
	std::unique_ptr<llvm::legacy::FunctionPassManager> FunctionPM;
	std::stack<LoopInfo> LoopStack;

};

} // end of namespace solidity
} // end of namespace dev
