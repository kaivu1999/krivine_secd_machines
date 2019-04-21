# Implementation of Krivine and SECD machines 

One can use the calculator interface for these two to use them.

To setup the file structue run the following command in the root directory :

```
make all
```

To use the calulator using krivine machine one may run
```
./calulate_krivine
```
To use the calulator using secd machine one may run
```
./calulate_secd
```
Syntax that I used can be understood from the exmamples shown.
To end a line or to calculate an expression use *;;* to end a line.

Some examples : 
```
==> 1 + 2 ;;
Answer: 3
==> 5 div 2 ;;
Answer: 2
==> 5 rem 2 ;;
Answer: 1
==> 1 + 2 * 3 ;;
Answer: 7
==> ( 1 + 2 ) * 3 ;;
Answer: 9
==> (\x.x + 1) (4);;
Answer: 5
==> X + 5 ;;
Answer: 10
==> if 5 > 6 then 1 else 2 fi ;;
Answer: 2
==> if ( if T then T else F fi ) then 3 else 4 fi ;; 
Answer: 3
==> 5 >= 5 ;;
Answer: true
==> 1 + T ;;
Answer: Fatal error: exception A1.Type_Error
```

The following examples were for calculator_krivine.