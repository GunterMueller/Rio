let rec fac x =
  if (equ x 0)
    1
    (mul x (fac (sub x 1)))
in fac 10

