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


void dev::solidity::LogError(const char *msg) {
	fprintf(stderr, "\n!!!Error: %s\n", msg);
	exit (1);
}

void dev::solidity::LogError(const char *msg, const char *content) {
	fprintf(stderr, "\n!!!Error: %s: %s\n", msg, content);
	exit (1);
}

void dev::solidity::LogError(const char *msg, string content) {
	fprintf(stderr, "\n!!!Error: %s: %s\n", msg, content.data());
	exit (1);
}

void dev::solidity::LogError(const char *msg, ASTNode const& node) {
	ASTPrinter printer(node);
	std::cerr << "\n!!! Error: " << msg << "\n";
	printer.print(std::cerr);
	std::cerr << endl;
	exit (1);
}

void dev::solidity::LogError(const char *msg, ASTNode const* node) {
	ASTPrinter printer(*node);
	std::cerr << "\n!!! Error: " << msg << "\n";
	printer.print(std::cerr);
	std::cerr << endl;
	exit (1);
}

void dev::solidity::LogError(const char *msg, Type const* type) {
	std::cerr << "\n!!! Error: " << msg << "\n";
	std::cerr << type->toString();
	std::cerr << endl;
	exit (1);
}

void dev::solidity::LogWarning(const char *msg) {
	fprintf(stderr, "\n!!! Warning: %s\n", msg);
}

void dev::solidity::LogDebug(string msg) {
	if (DebugLLVM)
		cout << "!! Debug: " << msg << endl;
}

void dev::solidity::LogDebug(string msg, ASTNode const& node) {
	if (DebugLLVM) {
		llvm::outs() << "!! Debug: " << msg;
		ASTPrinter printer(node);
		printer.print(std::cerr);
		std::cerr << endl;
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
