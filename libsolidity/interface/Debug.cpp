/*
	This file is not part of solidity.
*/
/**
 * @author Ta Quang Trung.
 * @date 2019
 * Versioning.
 */

#include <libsolidity/interface/Debug.h>

using namespace dev;
using namespace dev::solidity;
using namespace std;

bool dev::solidity::DebugLLVM = false;


void dev::solidity::LogError(string msg) {
	cerr<< "\n!!!Error: " << msg << endl;
	exit (1);
}

void dev::solidity::LogError(string msg, string content) {
	cerr<< "\n!!!Error: " << msg << ": " << content << endl;
	exit (1);
}

void dev::solidity::LogError(string msg, ASTNode const& node) {
	ASTPrinter printer(node);
	cerr << "\n!!! Error: " << msg << ": ";
	printer.print(cerr);
	cerr << endl;
	exit (1);
}

void dev::solidity::LogError(string msg, ASTNode const* node) {
	ASTPrinter printer(*node);
	cerr << "\n!!! Error: " << msg << ": ";
	printer.print(cerr);
	cerr << endl;
	exit (1);
}

void dev::solidity::LogError(string msg, Type const* type) {
	cerr << "\n!!! Error: " << msg << ": " << type->toString();
	cerr << endl;
	exit (1);
}

void dev::solidity::LogWarning(string msg) {
	cout << "\n!!! Warning: " << msg << endl;
}

void dev::solidity::LogDebug(string msg) {
	if (DebugLLVM)
		cout << "!! Debug: " << msg << endl;
}

void dev::solidity::LogDebug(string msg, string content) {
	if (DebugLLVM)
		cout << "!! Debug: " << msg << ": " << content << endl;
}

void dev::solidity::LogDebug(string msg, ASTNode const& node) {
	if (DebugLLVM) {
		llvm::outs() << "!! Debug: " << msg;
		ASTPrinter printer(node);
		printer.print(cerr);
		cerr << endl;
	}
}

void dev::solidity::LogDebug(string msg, llvm::Value* value) {
	if (DebugLLVM) {
		llvm::outs() << "!! Debug: " << msg;
		value->print(llvm::outs());
		llvm::outs() << "\n";
	}
}

void dev::solidity::LogDebug(string msg, llvm::Type* type) {
	if (DebugLLVM) {
		llvm::outs() << "!! Debug: " << msg;
		type->print(llvm::outs());
		llvm::outs() << "\n";
	}
}
