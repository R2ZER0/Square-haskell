import std.stdio;
import std.algorithm;
import std.string;
import std.conv;
import std.regex;

import util;
//import frame;
import value;


void execute(Frame frame, string code) {
    foreach(string statement; split(code,regex(";"))) {
        executeStatement(frame, statement);
    }
}

void executeStatement(Frame frame, string statement) {
    string[] words = filter!(a => !(a.length <= 0))(split(chompCompletely(statement), regex("\\s")).to!(string[]));
    string command = chompCompletely(words[0]);
    string[] args = words[1..$];
    
    frame.def("numargs", args.length);
    for(int i = 0; i < args.length; ++i) {
        frame.def("arg" ~ to!string(i), args[i]);
    }
    
    if(command in frame.defs) {
        // call the defs
    } else if (command in frame.builtin) {
        frame.builtin[command](frame);
    }
}

void executeBlock

void main(string[] args) {
    write("Square 0.01 REPL\n\n> ");
    
    string line;
    while ((line = stdin.readln()) !is null) {
        write("> ");
    }
}