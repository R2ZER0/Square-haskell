
class Frame {
    Frame parent;
    
    FunctionValue[string] functionValues;
    Value[string] values;
}

class Statement 

class ValueList : Value {
    public Value value;
    public ValueList next;
    
    string getString() { return "List"; }
    string getInt() { return 0; }
    string 
}


interface Value {
    string getString();
}

class NullValue {
    string getString() { return "NULL"; }
}

class LiteralValue {   
    private string literal immutable;
    public this(string literal) immutable {
        this.literal = literal;
    }
    
    string getString() { return this.literal; }
}


//// Dealing with Functions ////
interface FunctionValue : Value {
    void eval(Frame frame);
}


class NativeFunctionValue : FunctionValue {
    
    private immutable(void delegate(Frame)) dg;

    public this(void delegate(Frame) dg) immutable {
        this.dg = dg;
    }
    
    public void eval(Frame frame) {
        this.dg(frame);
    }
}

class ScriptFunctionValue : FunctionValue {
    
    private ValueList 