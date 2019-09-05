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

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;

namespace dev
{
namespace solidity
{

extern bool DebugLLVM;

void LogError(string msg);
void LogError(string msg, string content);
void LogError(string msg, ASTNode const& node);
void LogError(string msg, ASTNode const* node);
void LogError(string msg, Type const* type);
void LogWarning(string msg);
void LogDebug(string msg);
void LogDebug(string msg, string content);
void LogDebug(string msg, ASTNode const& node);
void LogDebug(string msg, llvm::Value* value);
void LogDebug(string msg, llvm::Type* type);

}
}
