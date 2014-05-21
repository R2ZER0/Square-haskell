import std.utf;
import std.uni;

class SquareVM {


    dstring[dstring] aliases;
    
    dstring code;
    
}

void exec(SquareVM vm) {
    if(vm.code.length = 0) {
        return;
    } else {
        exec(execStatement(code));
    }
}
    
void execStatement(SquareVM vm) {
    
    
}

void execStatementBody(string funName, string[] funArgs, SquareVM vm) {

}

