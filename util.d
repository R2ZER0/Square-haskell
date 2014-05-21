import std.conv;

string chompCompletely(string str) {
    string chomped = chompPrefix(chomp(str).to!string," ");
    if(chomped == str) {
        return str;
    } else {
        return chompCompletely(chomped);
    }
}