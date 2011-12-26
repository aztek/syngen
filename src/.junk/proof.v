Require Import List.
Require Import String.

Inductive Terminal : Set :=
  | t : string -> Terminal.

Inductive Nonterminal :Set :=
  | nt : string -> Nonterminal.

Inductive Symbol : Set :=
  | terminalSymbol : Terminal -> Symbol
  | nonterminalSymbol : Nonterminal -> Symbol.

Inductive CST : Set :=
  | leaf : Terminal -> CST
  | fork : Nonterminal -> list CST -> CST.

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x , .. , y ]" := (cons x .. (cons y nil) ..).

Definition s : list nat := [1, 2, 3].

Definition ArithExpr : CST :=
  fork (nt "expr")
       [fork (nt "operation")
             [(fork (nt "expr")
                    [fork (nt "number") [leaf (t "1")]]),
              (leaf (t "+")),
              (fork (nt "expr")
                    [fork (nt "operation")
                          [(fork (nt "expr")
                                 [fork (nt "number") [leaf (t "2")]]),
                           (leaf (t "*")),
                           (fork (nt "funcall")
                                 [(fork (nt "variable") [leaf (t "exp")]),
                                  (leaf (t "(")),
                                  (fork (nt "expr") [fork (nt "number") [leaf (t "4")]]),
                                  (leaf (t ")"))])]])]].

Fixpoint linearise (cst : CST) : list string :=
  match cst with
  | leaf (t s) => cons s nil
  | fork (nt n) csts => flat_map linearise csts
  end.

Eval simpl in linearise ArithExpr.

Inductive Constructor : Set :=
  | c : string -> Constructor.

Inductive ADTTree : Set :=
  | atom : Nonterminal -> ADTTree
  | compound : Constructor -> list ADTTree -> ADTTree.

Inductive Entry (K : Set) (V : Set) : Set :=
  | entry : K -> V -> Entry K V.

(*Inductive AList (K : Set) (V : Set) : list (Entry K V) :=.*)

Definition key_eq {K : Set} (key1 : K) (key2 : K) : bool :=
  true.

Fixpoint searchAList {K : Set} {V : Set} (needle : K) (haystack : list (Entry K V)) : option V :=
  match haystack with
  | (entry key val) :: haystack' => if key_eq key needle
                                      then Some val
                                      else searchAList needle haystack'
  | _ => None
  end.