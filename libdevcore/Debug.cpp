/*
	This file is not part of solidity.
*/
/**
 * @author Ta Quang Trung.
 * @date 2019
 * Versioning.
 */

#include <libdevcore/Debug.h>

using namespace dev;
using namespace dev::solidity;
using namespace std;

bool dev::solidity::DebugLLVM = false;

void dev::solidity::print(char const* msg) {
	cout << msg;
}

void dev::solidity::print(ASTNode const* node) {
	if (node == nullptr)
		cout << "(nullptr)";
	else {
		ASTPrinter printer(*node);
		printer.print(cout);
	}
}

void dev::solidity::print(Type const* type) {
	if (type == nullptr)
		cout << "(nullptr)";
	else
		cout << type->canonicalName();
}

void dev::solidity::print(llvm::Value* value) {
	if (value == nullptr)
		cout << "(nullptr)";
	else
		value->print(llvm::outs());
}

void dev::solidity::print(llvm::Type* type) {
	if (type == nullptr)
		cout << "(nullptr)";
	else
		type->print(llvm::outs());
}
