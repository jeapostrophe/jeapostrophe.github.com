(* This is a Coq program *)
Require Import List.

Inductive Num : Set :=
| Zero : Num
| Succ : Num -> Num.

Check Zero.
Check Succ Zero.

Variable X : Set.
Variable x : X.

(* Check Succ x. *)

Fixpoint add (x:Num) (y:Num) : Num :=
match x with
| Zero => y
| Succ x => Succ (add x y)
end.

Check (add Zero Zero).
Compute (add Zero Zero).

Definition One := Succ Zero.

Compute (add One One).

Theorem add_assoc:
  forall (x y z:Num),
    (add (add x y) z) = (add x (add y z)).
Proof.
  intros x.
  induction x as [|sx].
  simpl. reflexivity.
  intros y z. simpl.
  rewrite IHsx. reflexivity.
Qed.

Theorem add_zero_l:
  forall x,
    add Zero x = x.
Proof.
  simpl. reflexivity.
Qed.

Print add_assoc.

Inductive Addable : Num -> Num -> Num -> Prop :=
| ZeroL : forall x,
            Addable Zero x x
| ZeroR : forall x,
            Addable x Zero x.

Theorem addish:
  forall (x y:Num),
    (exists z:Num,
      Addable x y z) \/
    (forall z:Num,
     ~ Addable x y z).
Proof.
  intros x. induction x as [|sx].
  intros y. left. exists y. apply ZeroL.
  intros y. destruct y as [|sy].
  left. exists (Succ sx). apply ZeroR.
  right.
  intros z ADD.
  inversion ADD.
Qed.

Print addish.

Theorem addishc:
  forall (x y:Num),
    { z:Num |
      Addable x y z } +
    (forall z:Num,
     ~ Addable x y z).
Proof.
  intros x. induction x as [|sx].
  intros y. left. exists y. apply ZeroL.
  intros y. destruct y as [|sy].
  left. exists (Succ sx). apply ZeroR.
  right.
  intros z ADD.
  inversion ADD.
Qed.

Print addishc.

Set Extraction AccessOpaque.
Extraction addishc.

Software Foundations by Pierce
Certified Programming by Adam Chlipala

Theorem ex:
  forall (p:JavaProgram),
    exists (a:ASMProgram),
      semantics_in_Java p = semantics_in_ASM a.

CompCert

(*
Theorem add_sym:
  forall x y,
    add x y = add y x.
Proof.
  intros x. induction x as [|sx]; simpl.

  intros y. induction y as [|sy]; simpl; auto.
  rewrite <- IHsy. auto.

  intros y. rewrite IHsx.

Admitted.

Theorem add_zero_r:
  forall x,
    add x Zero = x.
Proof.
  intros x. rewrite add_sym. apply add_zero_l.
Qed.
*)
