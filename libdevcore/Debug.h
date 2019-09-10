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
void LogDebugCore(Arg arg) {
	if (DebugLLVM) {
		print(arg);
	}
}

template <typename Arg, typename... Rest>
void LogDebugCore(Arg arg, Rest... rest) {
	if (DebugLLVM) {
		LogDebugCore(arg);
		llvm::outs() << " ";
		LogDebugCore(rest...);
	}
}

template <typename Arg, typename... Rest>
void LogDebug(Arg arg, Rest... rest) {
  llvm::outs() << "!! ";
	LogDebugCore(arg, rest...);
	llvm::outs() << "\n";
}

// error reporting functions

template <typename Arg>
void LogErrorCore(Arg arg) {
  print(arg);
}

template <typename Arg, typename... Rest>
void LogErrorCore(Arg arg, Rest... rest) {
	LogErrorCore(arg);
	llvm::outs() << " ";
  LogErrorCore(rest...);
}

template <typename Arg, typename... Rest>
void LogError(Arg arg, Rest... rest) {
  llvm::outs() << "@@ Error: ";
	LogErrorCore(arg, rest...);
	llvm::outs() << "\n";
	exit(1);
}


}
}
