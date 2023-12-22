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
      assert_equal_atom "NSString.self" (TypeRef (SimpleType ("NSString")));
    "class reference .class" >::
      assert_equal_atom "NSString.class" (TypeRef (SimpleType ("NSString")));
    "selector" >::
      assert_equal_atom "@selector(methodWithParam:)" (Selector "methodWithParam:");
    "self" >::
      assert_equal_atom "self" Self;
    "nil" >::
      assert_equal_atom "nil" NoValue;
    "NULL" >::
      assert_equal_atom "NULL" NoValue
  ]

let suite =
  "parser" >:::
    List.flatten [
        literal_tests;
        atom_tests;
      ]

let _ = run_test_tt_main suite
