(* Sorting, proven *)

(* List a = Empty | Cons a (List a) *)

Inductive List (a:Type) : Type :=
| Empty : List a
| Cons : a -> List a -> List a.

Definition example1 : (List nat) :=
  Cons nat 0 (Cons nat 1 (Empty nat)).

Inductive NList : Type :=
| NEmpty : NList
| NCons : nat -> NList -> NList.

Definition example1' : NList :=
  NCons 0 (NCons 1 (NEmpty)).

Print example1'.

Require Import Arith.

Fixpoint insert (n:nat) (l:NList)
  : NList :=
  match l with
    | NEmpty =>
      NCons n NEmpty
    | NCons fst rst =>
      if leb n fst then
        NCons n l
      else
        NCons fst (insert n rst)
end.

Fixpoint isort (l:NList) : NList :=
  match l with
    | NEmpty =>
      NEmpty
    | NCons fst rst =>
      insert fst (isort rst)
  end.

Eval compute in (isort example1').
Eval compute in (isort (NCons 1 (NCons 0 NEmpty))).

(* Every element of l is greater than or equal to the one preceeding it *)
(* Definition Sorted_try1 (l:NList) : Prop :=
  forall n m,
    (nth l n) <= (nth l (n+m)).
    *)

(* Everything in side of the list is bigger than the first argument and is also sorted *)
Inductive nat_Sorted : nat -> NList -> Prop :=
| nS_NEmpty :
  forall n,
    nat_Sorted n NEmpty
| nS_NCons :
  forall n fst rst,
    n <= fst ->
    nat_Sorted fst rst ->
    nat_Sorted n (NCons fst rst).
Hint Constructors nat_Sorted.

Definition Sorted (l:NList) : Prop :=
  nat_Sorted 0 l.
Hint Unfold Sorted.

Lemma nat_Sorted_invert1:
  forall m fst rst,
    nat_Sorted m (NCons fst rst) ->
    m <= fst.
Proof.
 (*
 intros m fst rst NS.
 inversion NS. subst.
 assumption. *)

 intros. inversion H. subst. auto.

Qed.
Hint Resolve nat_Sorted_invert1.

Lemma nat_Sorted_invert2:
  forall m fst rst,
    nat_Sorted m (NCons fst rst) ->
    nat_Sorted fst rst.
Proof.
 intros m fst rst NS.
 inversion NS. subst.
 assumption.
Qed.
Hint Resolve nat_Sorted_invert2.

Lemma nat_Sorted_extends:
  forall m n fst rst,
    nat_Sorted m (NCons fst rst) ->
    m <= n ->
    n <= fst ->
    nat_Sorted n (NCons fst rst).
Proof.
 (* intros m n fst rst.
    intros NS1 LEmn LEnf. *)
  intros.
 eauto.
 (*
 apply nS_NCons; auto.
  eapply nat_Sorted_invert2.
  apply NS1. *)
Qed.

Lemma insert_correct :
  forall n l m,
    nat_Sorted m l ->
    m <= n ->
    nat_Sorted m (insert n l).
Proof.
  intros n l.
  induction l.

  intros. simpl. eauto.
  (*
  intros m SORTEDl LEmn.
  simpl.
  unfold Sorted.
  apply nS_NCons.
   apply LEmn.
   apply nS_NEmpty. *)

  intros m SORTEDl LEmn.
  rename n0 into fst.
  rename l into rst.
  simpl.
  remember (leb n fst) as leb_result.
  destruct leb_result.

  (* true side *)
  SearchAbout leb true.
  symmetry in Heqleb_result.
  rewrite leb_iff in Heqleb_result.
  unfold Sorted.
  apply nS_NCons.
   apply LEmn.
   unfold Sorted in SORTEDl.
   apply nat_Sorted_extends with (m:=m).
    apply SORTEDl.
    apply LEmn.
    apply Heqleb_result.

  (* false side *)
  symmetry in Heqleb_result.
  SearchAbout leb.
  rewrite leb_iff_conv in Heqleb_result.
  apply nS_NCons.
   apply nat_Sorted_invert1 with (rst:=rst).  
    apply SORTEDl.
  apply IHl.
   apply nat_Sorted_invert2 with (m:=m).
   apply SORTEDl.

 SearchAbout le lt.
   apply lt_le_weak.
   apply Heqleb_result.
Qed.
   
Theorem isort_correct :
  forall l,
    Sorted (isort l).
Proof.
  intros l.
  unfold Sorted.
  induction l.

  simpl.
  apply nS_NEmpty.

  simpl.
  Print insert_correct.
  apply insert_correct.
  clear n.
  unfold Sorted.
  exact IHl.
  apply le_0_n.
Qed.

(*
Fixpoint insert' (n:nat) (l:NList)
  (SORTEDl:exists m, nat_Sorted m l)
  : { r : NList | nat_Sorted n r } :=
  match l with
    | NEmpty =>
      NCons n NEmpty
    | NCons fst rst =>
      if leb n fst then
        NCons n l
      else
        NCons fst (insert' n rst)
end.
*)

Definition isort' (l:NList) :=
  NEmpty.

Theorem isort'_correct :
  forall l,
    Sorted (isort' l).
Proof.
 intros l.
 unfold Sorted.
 eauto.
Qed.

(* Permutation l l' *)
