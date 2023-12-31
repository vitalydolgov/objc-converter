# Objective-C to Swift converter

**WIP**, but you can receive some benefit from it, even if it's far from being complete.

The primary target of the project is to convert methods, so you can partially migrate Objective-C code to extensions in Swift, and with bridge publish them back to Objective-C runtime. With this approach it's possible to start and stop refactoring at any time.

## Example

A method like the following will be converted to corresponding Swift version.

```
- (NSArray<NSString *> *)getDigitsOfNumber:(NSInteger)value {
    // code
}

 ↓↓

@objc public func getDigits(ofNumber value: Int) -> [String] {
    // code
}
```

For now there is no magic behind and rules for translation are not at all complicated. Basically, it creates a draft version of a method that you need to adopt manually. It's also possible to convert just one statement or expression by wrapping it in `!{ ... }`, statements *should be* ended with semicolon. 

```
!{
  NSString *str = @"Hello, world!";
}
```

## Building

```
% cd path/to/objc_converter
% brew install opam
% opam pin .
% dune build
```

To build a dynamic library `libobjcconv.so` use the following command
```
% dune build @export
% file _build/default/libobjcconv/libobjcconv.so
```

## Using

Put one or several Objective-C methods to a file, e.g. `program.txt`, then run with

```
% dune exec bin/main.exe program.txt
```

For AST use `--dump`. You can also process whole implementation, that is everything between `@implementation` and `@end` including these introducers.
