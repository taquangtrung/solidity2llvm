#pragma once

#include "libsolidity/parsing/Scanner.h"
#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTPrinter.h"
#include "libsolidity/ast/ASTVisitor.h"
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

#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

#include <map>

using namespace std;

using Value = llvm::Value;

namespace dev {

namespace solidity {

class LlvmCompiler {

public:
  explicit LlvmCompiler():
  CompilingContract(nullptr)
  { }


  void compileContract(const ContractDefinition* contract);

  // compile declarations
  llvm::StructType* compileStructDecl(const StructDefinition* st);
  Value* compileGlobalVarDecl(const VariableDeclaration* var);
  Value* compileLocalVarDecl(VariableDeclaration& var);
  Value* compileLocalVarDecl(VariableDeclaration& var, const Expression* value);
  llvm::Function* compileFunc(FunctionDefinition const* func);

  // compile statements
  Value* compileStmt(Statement const& stmt);
  Value* compileStmt(InlineAssembly const* stmt);
  Value* compileStmt(Block const* stmt);
  Value* compileStmt(PlaceholderStatement const* stmt);
  Value* compileStmt(IfStatement const* stmt);
  Value* compileStmt(BreakableStatement const* stmt);
  Value* compileStmt(WhileStatement const* stmt);
  Value* compileStmt(ForStatement const* stmt);
  Value* compileStmt(Continue const* stmt);
  Value* compileStmt(Break const* stmt);
  Value* compileStmt(Return const* stmt);
  Value* compileStmt(Throw const* stmt);
  Value* compileStmt(EmitStatement const* stmt);
  Value* compileStmt(VariableDeclarationStatement const* stmt);
  Value* compileStmt(ExpressionStatement const* stmt);

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
  string stringOf(Value* value);
  Value* findNamedValue(string name);

private:
  const ContractDefinition* CompilingContract;
  StringMap compilingSourceCodes;

};

} // end of namespace solidity
} // end of namespace dev
