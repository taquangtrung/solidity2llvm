/*
	This file is not a part of Solidity.
*/
/**
 * @author Ta Quang Trung.
 * @date 2019
 * Debugging
 */

#pragma once

#include <libdevcore/Common.h>

#include "libsolidity/ast/AST.h"
#include "libsolidity/ast/ASTPrinter.h"

#include <llvm/IR/Type.h>
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;

namespace dev
{
namespace solidity
{

extern bool DebugLLVM;

void LogDebug(string msg);
void LogDebug(string msg, string content);
void LogDebug(string msg, ASTNode const& node);
void LogDebug(string msg, ASTNode const* node);
void LogDebug(string msg, Type const* type);
void LogDebug(string msg, llvm::Value* value);
void LogDebug(string msg, llvm::Type* type);

void LogWarning(string msg);

void LogError(string msg);
void LogError(string msg, string content);
void LogError(string msg, ASTNode const& node);
void LogError(string msg, ASTNode const* node);
void LogError(string msg, Type const* type);

// template <typename Arg>
// void print(Arg arg) {
// 	if (auto v = dynamic_cast<ASTNode const*>(arg)) {
// 		ASTPrinter printer(*v);
// 		printer.print(cout);
// 		return;
// 	}

// 	if (auto v = dynamic_cast<Expression const*>(arg)) {
// 		ASTPrinter printer(*v);
// 		printer.print(cout);
// 		return;
// 	}

// 	if (auto v = dynamic_cast<Type const*>(arg)) {
// 		cout << v->canonicalName();
// 		return;
// 	}
// }

// template <typename Arg, typename... Rest>
// void print(Arg arg, Rest... rest) {
// 	print(arg);
// 	print(rest...);
// }

}
}
