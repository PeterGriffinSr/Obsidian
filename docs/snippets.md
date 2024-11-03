# Front page snippets for Obsidian

## Fibonacci
A sample of fibonacci sequence in Obsidian.
```obsidian
fn fib(int n) int {
    if (n <= 1) {
        return n;
    }
    int a = 0;
    int b = 1;
    int temp;

    for (int i = 2; i <= n; i++) {
        temp = a + b;
        a = b;
        b = temp;
    }
    return b;
}

fn main() int {
    println(fib(92));
    return 0;
}
```
## C++
```cpp
#include <iostream>
using namespace std;

int fib(int n) {
    if (n <= 1) {
        return n;
    }

    int a = 0, b = 1, c;

    for (int i = 2; i <= n; i++) {
        c = a + b;
        a = b;
        b = c;
    }

    return b;
}

int main() {
    cout << fib(92) << endl;

    return 0;
}
```