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

// global variables

extern bool DebugLLVM;

// printing functions

extern void print(char const*);
extern void print(ASTNode const*);
extern void print(Type const*);
extern void print(llvm::Value*);
extern void print(llvm::Type*);

// debugging functions

template <typename Arg>
void LogDebug(Arg arg) {
	if (DebugLLVM) {
		print(arg);
	}
}

template <typename Arg, typename... Rest>
void LogDebug(Arg arg, Rest... rest) {
	LogDebug(arg);
	LogDebug(rest...);
}

// error reporting functions

template <typename Arg>
void LogError(Arg arg) {
  print(arg);
}

template <typename Arg, typename... Rest>
void LogError(Arg arg, Rest... rest) {
	LogError(arg);
	if (sizeof...(rest) == 0)
		exit(1);
	else
		LogError(rest...);
}

}
}
