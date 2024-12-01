theory Program
imports Main
        N
        M_N
        M_Main
begin

definition id0 :: "nat \<Rightarrow> nat" where
  "id0 = id"

definition id1 :: "nat list \<Rightarrow> nat list" where
  "id1 = id"

definition id2 :: "'A \<Rightarrow> 'A" where
  "id2 = id"

(* Add one to each element in a list *)
fun add_one :: "nat list \<Rightarrow> nat list" where
  "add_one [] = []" |
  (* hello! *)
  "add_one (x' # xs) = ((x' + 1) # add_one xs)"

fun sum :: "nat list \<Rightarrow> nat" where
  "sum [] = 0" |
  "sum (x' # xs) = (x' + sum xs)"

fun f :: "nat \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> nat" where
  "f x' y z = ((z + 1) * x' + y)"

fun g :: "nat \<Rightarrow> nat \<Rightarrow> bool" where
  "g x' y = (if x' = f x' (N.g y) (M_N.g (M_Main.f y)) then False else True)"

fun inc :: "nat \<Rightarrow> nat" where
  "inc x' = (Suc x')"

(* dec function *)
fun dec :: "nat \<Rightarrow> nat" where
  "dec 0 = 0" |
  "dec (Suc x') = x'"

(* dec' function *)
fun dec' :: "nat \<Rightarrow> nat" where
  (* Do case switch *)
  (* pattern match on x *)
  (* the zero case  *)
  (* return zero  *)
  (* the suc case  *)
  "dec' x' =
    (case x' of
       0 \<Rightarrow> 0 |
       (Suc y) \<Rightarrow> y)"

fun optmap :: "('A \<Rightarrow> 'A) \<Rightarrow> 'A option \<Rightarrow> 'A option" where
  "optmap f' None = None" |
  "optmap f' (Some x') = (Some (f' x'))"

fun pboth :: "('A \<Rightarrow> 'A') \<Rightarrow> ('B \<Rightarrow> 'B') \<Rightarrow> 'A \<times> 'B \<Rightarrow> 'A' \<times> 'B'" where
  "pboth f' g' (x', y) = (f' x', g' y)"

fun bool_fun :: "bool \<Rightarrow> bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bool_fun x' y z = (x' \<and> (y \<or> z))"

fun bool_fun' :: "bool \<Rightarrow> bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bool_fun' x' y z = (x' \<and> y \<or> z)"

(* Queues *)
text \<open>
A type of Queues
\<close>
datatype 'A Queue
  = queue "'A list" "'A list"

fun qfst :: "'A Queue \<Rightarrow> 'A list" where
  "qfst (queue x' v') = x'"

fun qsnd :: "'A Queue \<Rightarrow> 'A list" where
  "qsnd (queue v' v'0) = v'0"

fun pop_front :: "'A Queue \<Rightarrow> 'A Queue" where
  "pop_front q =
    (let
       q' = queue (tl (qfst q)) (qsnd q)
     in case qfst q' of
          [] \<Rightarrow> queue (rev (qsnd q')) [] |
          v' \<Rightarrow> q')"

fun push_back :: "'A Queue \<Rightarrow> 'A \<Rightarrow> 'A Queue" where
  "push_back q x' =
    (case qfst q of
       [] \<Rightarrow> queue [x'] (qsnd q) |
       q' \<Rightarrow> queue q' (x' # qsnd q))"

text \<open>
Checks if the queue is empty
\<close>
fun is_empty :: "'A Queue \<Rightarrow> bool" where
  "is_empty q =
    (case qfst q of
       [] \<Rightarrow>
         (case qsnd q of
            [] \<Rightarrow> True |
            v' \<Rightarrow> False) |
       v' \<Rightarrow> False)"

definition empty :: "'A Queue" where
  "empty = queue [] []"

(* Multiple let expressions *)
fun funkcja :: "nat \<Rightarrow> nat" where
  "funkcja n =
    (let
       nat1 = 1;
       nat2 = 2;
       plusOne = \<lambda> x0 . x0 + 1
     in plusOne n + nat1 + nat2)"

text \<open>
An Either' type
\<close>
datatype ('A, 'B) Either'
  = (* Left constructor *)
    Left' 'A |
    (* Right constructor *)
    Right' 'B

(* Records *)
record R =
  r1 :: nat
  r2 :: nat

fun r1 :: "R \<Rightarrow> nat" where
  "r1 (| R.r1 = r1'0, R.r2 = r2'0 |) = r1'0"

fun r2 :: "R \<Rightarrow> nat" where
  "r2 (| R.r1 = r1'0, R.r2 = r2'0 |) = r2'0"

definition r :: R where
  "r = (| R.r1 = 0, R.r2 = 1 |)"

definition v :: nat where
  "v = 0"

fun funR :: "R \<Rightarrow> R" where
  "funR r' = (r' (| R.r1 := R.r1 r' + R.r2 r' |))"

fun funRR :: "R \<Rightarrow> R" where
  "funRR r'0 = (r'0 (| R.r1 := R.r1 r'0 + R.r2 r'0 |))"

fun funR' :: "R \<Rightarrow> R" where
  "funR' (| R.r1 = rr1, R.r2 = rr2 |) =
    (let
       r1'0 = rr1 + rr2;
       r2'0 = rr2
     in (| R.r1 = r1'0, R.r2 = r2'0 |))"

fun funR1 :: "R \<Rightarrow> R" where
  "funR1 (| R.r1 = 0, R.r2 = r2'0 |) =
    (let
       r1'0 = r2'0;
       r2'1 = r2'0
     in (| R.r1 = r1'0, R.r2 = r2'1 |))" |
  "funR1 (| R.r1 = rr1, R.r2 = rr2 |) =
    (let
       r1'0 = rr2;
       r2'0 = rr1
     in (| R.r1 = r1'0, R.r2 = r2'0 |))"

fun funR2 :: "R \<Rightarrow> R" where
  "funR2 r' =
    (case (R.r1 r') of
       (0) \<Rightarrow>
         let
           r1'0 = R.r2 r';
           r2'0 = R.r2 r'
         in (| R.r1 = r1'0, R.r2 = r2'0 |) |
       _ \<Rightarrow>
         let
           r1'0 = R.r2 r';
           r2'0 = R.r1 r'
         in (| R.r1 = r1'0, R.r2 = r2'0 |))"

fun funR3 :: "(R, R) Either' \<Rightarrow> R" where
  "funR3 er =
    (case er of
       (Left' v') \<Rightarrow>
         (case (R.r1 v') of
            (0) \<Rightarrow>
              let
                r1'0 = R.r2 v';
                r2'0 = R.r2 v'
              in (| R.r1 = r1'0, R.r2 = r2'0 |) |
            _ \<Rightarrow>
              (case er of
                 (Left' v'1) \<Rightarrow>
                   let
                     r1'0 = R.r2 v'1;
                     r2'0 = R.r1 v'1
                   in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                 v'2 \<Rightarrow>
                   (case v'2 of
                      (Right' v'1) \<Rightarrow>
                        (case (R.r2 v'1) of
                           (0) \<Rightarrow>
                             let
                               r1'0 = 7;
                               r2'0 = 7
                             in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                           _ \<Rightarrow>
                             (case v'2 of
                                (Right' r') \<Rightarrow>
                                  r' (| R.r1 := R.r2 r' + 2, R.r2 := R.r1 r' + 3 |))) |
                      v'4 \<Rightarrow>
                        (case v'4 of
                           (Right' r') \<Rightarrow>
                             r' (| R.r1 := R.r2 r' + 2, R.r2 := R.r1 r' + 3 |))))) |
       v'2 \<Rightarrow>
         (case v'2 of
            (Right' v'1) \<Rightarrow>
              (case (R.r2 v'1) of
                 (0) \<Rightarrow>
                   let
                     r1'0 = 7;
                     r2'0 = 7
                   in (| R.r1 = r1'0, R.r2 = r2'0 |) |
                 _ \<Rightarrow>
                   (case v'2 of
                      (Right' r') \<Rightarrow>
                        r' (| R.r1 := R.r2 r' + 2, R.r2 := R.r1 r' + 3 |))) |
            v'4 \<Rightarrow>
              (case v'4 of
                 (Right' r') \<Rightarrow>
                   r' (| R.r1 := R.r2 r' + 2, R.r2 := R.r1 r' + 3 |))))"

fun funR4 :: "R \<Rightarrow> R" where
  "funR4 r'0 = (r'0 (| R.r2 := R.r1 r'0 |))"

(* Standard library *)
fun bf :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
  "bf b1 b2 = (\<not> (b1 \<and> b2))"

fun nf :: "int \<Rightarrow> int \<Rightarrow> bool" where
  "nf n1 n2 = (n1 - n2 \<ge> n1 \<or> n2 \<le> n1 + n2)"

(* Nested record patterns *)
record 'MessageType MessagePacket =
  target :: nat
  mailbox :: "nat option"
  message :: 'MessageType

record 'MessageType EnvelopedMessage =
  sender :: "nat option"
  packet :: "'MessageType MessagePacket"

record 'HandleType Timer =
  time :: nat
  handle :: 'HandleType

datatype ('MessageType, 'HandleType) Trigger
  = MessageArrived "'MessageType EnvelopedMessage" |
    Elapsed "('HandleType Timer) list"

fun target :: "'MessageType MessagePacket \<Rightarrow> nat" where
  "target (| MessagePacket.target = target', MessagePacket.mailbox = mailbox', MessagePacket.message = message' |) =
    target'"

fun mailbox :: "'MessageType MessagePacket \<Rightarrow> nat option" where
  "mailbox (| MessagePacket.target = target', MessagePacket.mailbox = mailbox', MessagePacket.message = message' |) =
    mailbox'"

fun message :: "'MessageType MessagePacket \<Rightarrow> 'MessageType" where
  "message (| MessagePacket.target = target', MessagePacket.mailbox = mailbox', MessagePacket.message = message' |) =
    message'"

fun sender :: "'MessageType EnvelopedMessage \<Rightarrow> nat option" where
  "sender (| EnvelopedMessage.sender = sender', EnvelopedMessage.packet = packet' |) =
    sender'"

fun packet :: "'MessageType EnvelopedMessage \<Rightarrow> 'MessageType MessagePacket" where
  "packet (| EnvelopedMessage.sender = sender', EnvelopedMessage.packet = packet' |) =
    packet'"

fun time :: "'HandleType Timer \<Rightarrow> nat" where
  "time (| Timer.time = time', Timer.handle = handle' |) = time'"

fun handle :: "'HandleType Timer \<Rightarrow> 'HandleType" where
  "handle (| Timer.time = time', Timer.handle = handle' |) = handle'"

fun getMessageFromTrigger :: "('M, 'H) Trigger \<Rightarrow> 'M option" where
  "getMessageFromTrigger v_0 =
    (case (v_0) of
       (MessageArrived v') \<Rightarrow>
         (case (EnvelopedMessage.packet v') of
            (v'0) \<Rightarrow> Some (MessagePacket.message v'0)) |
       v'1 \<Rightarrow> None)"

fun getMessageFromTrigger' :: "('M, 'H) Trigger \<Rightarrow> 'M option" where
  "getMessageFromTrigger' t' =
    (case t' of
       (MessageArrived v') \<Rightarrow>
         (case (EnvelopedMessage.packet v') of
            (v'0) \<Rightarrow> Some (MessagePacket.message v'0)) |
       v'2 \<Rightarrow> None)"

(* Syntax aliases *)
type_synonym Name = nat

fun idT :: "nat \<Rightarrow> Name" where
  "idT x' = x'"

definition t :: Name where
  "t = 0"

record RR =
  x :: nat

(* Type constructor identifiers *)
record ('A, 'L, 'X) GuardOutput =
  args :: "'A list"
  label :: 'L
  other :: 'X

datatype GuardReturnLabel
  = doIncrement |
    doRespond nat

datatype GuardReturnOther
  = nuthing

datatype GuardReturnArgs
  = ReplyTo nat

fun x :: "RR \<Rightarrow> Name" where
  "x (| RR.x = x' |) = x'"

fun args :: "('A, 'L, 'X) GuardOutput \<Rightarrow> 'A list" where
  "args (| GuardOutput.args = args', GuardOutput.label = label', GuardOutput.other = other' |) =
    args'"

fun label :: "('A, 'L, 'X) GuardOutput \<Rightarrow> 'L" where
  "label (| GuardOutput.args = args', GuardOutput.label = label', GuardOutput.other = other' |) =
    label'"

fun other :: "('A, 'L, 'X) GuardOutput \<Rightarrow> 'X" where
  "other (| GuardOutput.args = args', GuardOutput.label = label', GuardOutput.other = other' |) =
    other'"

fun ifIncrement :: "(nat, nat) Trigger \<Rightarrow> ((GuardReturnArgs, GuardReturnLabel, GuardReturnOther) GuardOutput) option" where
  "ifIncrement (MessageArrived m) =
    (Some (let
             args' = [];
             label' = doIncrement;
             other' = nuthing
           in (| GuardOutput.args = args', GuardOutput.label = label', GuardOutput.other = other' |)))" |
  "ifIncrement (Elapsed ts) = None"

end
