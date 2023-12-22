open Common
open Objc_converter.Ast

let literal_tests = [
    "int" >::
      assert_equal_literal "1" (Int 1);
    "negative int" >::
      assert_equal_literal "-1" (Int (-1));
    "negative int w/ space" >::
      assert_equal_literal "- 1" (Int (-1));
    "float" >::
      assert_equal_literal "1.1" (Float 1.1);
    "float w/o decimal" >::
      assert_equal_literal "1." (Float 1.);
    "explicit float" >::
      assert_equal_literal "1.1f" (Float 1.1);
    "explicit float w/o decimal" >::
      assert_equal_literal "1.f" (Float 1.);
    "negative float" >::
      assert_equal_literal "-1.1" (Float (Float.neg 1.1));
    "negative float w/ space" >::
      assert_equal_literal "- 1.1" (Float (Float.neg 1.1));
    "string" >::
      assert_equal_literal "@\"abc\"" (String "abc");
    "boolean true" >::
      assert_equal_literal "YES" (Bool true);
    "boolean false" >::
      assert_equal_literal "NO" (Bool false);
    "object boolean true" >::
      assert_equal_literal "@YES" (Bool true);
    "object boolean false" >::
      assert_equal_literal "@NO" (Bool false)
  ]

let atom_tests = [
    "identifier" >::
      assert_equal_atom "myIdentifier_1" (Ident "myIdentifier_1");
    "identifier w/ underscore" >::
      assert_equal_atom "_myIdentifier_1" (Ident "_myIdentifier_1");
    "class reference .self" >::
      assert_equal_atom "NSString.self" (TypeRef (ObjectType "NSString"));
    "class reference .class" >::
      assert_equal_atom "NSString.class" (TypeRef (ObjectType "NSString"));
    "selector" >::
      assert_equal_atom "@selector(methodWithParam:)" (Selector "methodWithParam:");
    "self" >::
      assert_equal_atom "self" Self;
    "nil" >::
      assert_equal_atom "nil" NoValue;
    "NULL" >::
      assert_equal_atom "NULL" NoValue;
  ]

let assign_tests =
  let one = Atom (Literal (Int 1)) in [
    "regular assignment" >::
      assert_equal_assign "x = 1"
        (Assign (ValAssign, Atom (Ident "x"), one));
    "increment assignment" >::
      assert_equal_assign "x += 1"
        (Assign (IncAssign, Atom (Ident "x"), one));
    "decrement assignment" >::
      assert_equal_assign "x -= 1"
        (Assign (DecAssign, Atom (Ident "x"), one));
  ]

let newvar_tests =
  let one = (Literal (Int 1)) in [
    "variable of primitive type w/o initial value" >::
      assert_equal_newvar "NSInteger x;"
        (NewVar (NoOwnership, (PrimitiveType "NSInteger"), "x", (Atom NoValue)));
    "variable of primitive type" >::
      assert_equal_newvar "NSInteger x = 1;"
        (NewVar (NoOwnership, (PrimitiveType "NSInteger"), "x", (Atom one)));
    "variable of reference type w/o initial value" >::
      assert_equal_newvar "NSNumber *x;"
        (NewVar (Strong, (Optional (ObjectType "NSNumber")), "x", (Atom NoValue)));
    "variable of reference type" >::
      assert_equal_newvar "NSNumber *x = @1;"
        (NewVar (Strong, (Optional (ObjectType "NSNumber")), "x", (Atom one)));
    "variable of reference type _Nonnull" >::
      assert_equal_newvar "NSNumber * _Nonnull x = @1;"
        (NewVar (Strong, (ObjectType "NSNumber"), "x", (Atom one)));
    "variable of reference type _Nonnull" >::
      assert_equal_newvar "NSNumber * _Nullable x = @1;"
        (NewVar (Strong, (Optional (ObjectType "NSNumber")), "x", (Atom one)));
    "variable with weak ownership" >::
      assert_equal_newvar "__weak NSNumber *x = @1;"
        (NewVar (Weak, (Optional (ObjectType "NSNumber")), "x", (Atom one)));
    "variable of NSArray" >::
      assert_equal_newvar "NSArray<NSNumber *> *arr = @[];"
        (NewVar (Strong, (Optional
                            (Array (Optional (ObjectType "NSNumber")))),
                 "arr", (ArrayValues [])));
    "variable of NSMutableArray" >::
      assert_equal_newvar "NSMutableArray<NSNumber *> *arr = @[];"
        (NewVar (Strong, (Optional
                            (Array (Optional (ObjectType "NSNumber")))),
                 "arr", (ArrayValues [])));
    "variable of NSDictionary" >::
      assert_equal_newvar "NSDictionary<NSNumber *, NSString *> *dic = @{};"
        (NewVar (Strong, (Optional
                            (Dictionary ((ObjectType "NSNumber"), (Optional (ObjectType "NSString"))))),
                 "dic", (DictionaryValues [])));
    "variable of generic type" >::
      assert_equal_newvar "NSSet<NSNumber *> *obj;"
        (NewVar (Strong, (Optional
                            (GenericType (ObjectType "NSSet", [(Optional (ObjectType "NSNumber"))]))),
                 "obj", (Atom NoValue)));
    ]

let suite =
  "parser" >:::
    List.flatten [
        literal_tests;
        atom_tests;
        assign_tests;
        newvar_tests;
      ]

let _ = run_test_tt_main suite
