module Foo;

int foo(int y, string s) {
    y = y + 1;
    return mixin(s ~ "return y;");
}

pragma(msg, foo(3, "y = y*2;"));
