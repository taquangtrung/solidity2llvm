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
#include "libdevcore/Debug.h"
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

using LLContext = llvm::LLVMContext;
using LLModule = llvm::Module;
using LLFunction = llvm::Function;
using LLBlock = llvm::BasicBlock;
using LLValue = llvm::Value;
using LLConstant = llvm::Constant;
using LLConstantInt = llvm::ConstantInt;
using LLType = llvm::Type;
using LLArrayType = llvm::ArrayType;
using LLIntegerType = llvm::IntegerType;
using LLStructType = llvm::StructType;
using LLFunctionType = llvm::FunctionType;
using LLGlobalVar = llvm::GlobalVariable;

namespace dev {

namespace solidity {

typedef struct _LoopInfo {
	LLBlock* loopHead;
	LLBlock* loopEnd;
} LoopInfo;

class LlvmCompiler {

 public:
	explicit LlvmCompiler():
	CompilingContract(nullptr), Builder(Context) { }

	void compileContract(ContractDefinition const* contract);

	// declarations
	LLStructType* compileStructDecl(StructDefinition const*);
	LLIntegerType* compileEnumDecl(EnumDefinition const*);
	LLValue* compileGlobalVarDecl(VariableDeclaration const*);
	LLValue* compileLocalVarDecl(VariableDeclaration&, Expression const*);
	LLFunction* compileFuncDecl(FunctionDefinition const*);

	// block
	LLBlock* createLlvmBlock(string, LLFunction*);

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
	LLValue* compileExp(Expression const*);
	LLValue* compileExp(Conditional const*);
	LLValue* compileExp(Assignment const*);
	LLValue* compileExp(TupleExpression const*);
	LLValue* compileExp(UnaryOperation const*);
	LLValue* compileExp(BinaryOperation const*);
	LLValue* compileExp(FunctionCall const*);
	LLValue* compileExp(NewExpression const*);
	LLValue* compileExp(MemberAccess const*);
	LLValue* compileExp(IndexAccess const*);
	LLValue* compileExp(PrimaryExpression const*);
	LLValue* compileExp(Identifier const*);
	LLValue* compileExp(ElementaryTypeNameExpression const*);
	LLValue* compileExp(Literal const*);

	// special compilation
	vector<LLValue*> compileTupleExp(TupleExpression const*);

	// auxiliary LLVM functions
	LLValue* compileRhsExp(Expression const*);

	// compile types
	LLType* compileTypeName(TypeName const*);
	LLType* compileTypeName(ElementaryTypeName const*);
	LLType* compileTypeName(UserDefinedTypeName const*);
	LLType* compileTypeName(FunctionTypeName const*);
	LLType* compileTypeName(Mapping const*);
	LLType* compileTypeName(ArrayTypeName const*);
	LLType* compileType(TypePointer);

	// compile a contract to string.
	string llvmString(const ContractDefinition* contract, StringMap sourceCodes);

	// supporting functions
	string stringOf(LLModule*);
	string stringOf(LLFunction*);
	string stringOf(LLBlock*);
	string stringOf(LLValue*);
	string stringOf(LLType*);
	LLValue* findNamedValue(string);

private:
	const ContractDefinition* CompilingContract;
	StringMap compilingSourceCodes;

private:
	LLContext Context;
	llvm::IRBuilder<> Builder;
	unique_ptr<LLModule> Module;
	string ContractName;
	unique_ptr<llvm::legacy::FunctionPassManager> FunctionPM;
	stack<LoopInfo> LoopStack;

	// global vars for module
	map<string, LLValue*> MapGlobalVars;
	map<string, LLStructType*> MapStructTypes;
	map<string, map<string, int> > MapEnumTypes;
	map<string, LLType*> MapFuncReturnType;
	set<LLValue*> SetGlobalVars;

	// global vars for function
	map<string, LLValue*> MapLocalVars;
	set<LLValue*> SetLocalVars;
	int IndexBlock;
};

} // end of namespace solidity
} // end of namespace dev