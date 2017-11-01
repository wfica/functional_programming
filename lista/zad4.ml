(*
Zadanie 4 (3p.)

Funkcja jest napisana w stylu przekazywania kontynuacji (continuation-passing style, CPS), jeżeli 
przyjmuje dodatkowy argument funkcyjny zwany kontynuacją, który reprezentuje całą resztę 
obliczeń jakie mają zostać przeprowadzone po powrocie z tej funkcji. W konsekwencji, funkcje w 
CPS-ie zwracają wynik wywołując swoją kontynuację, a wszystkie wywołania w programie w CPS-ie 
są ogonowe. Na przykład, funkcja licząca silnię napisana w CPS-ie wygląda następująco: 

let rec fact_cps n k = 
  if n = 0 
  then k 1 
  else fact_cps (n-1) (fun v -> k (n*v))
  
Kontynuacja początkowa przekazana funkcji fact_cps mówi co zrobić z wynikiem obliczenia silni 
zadanej liczby. Typowe wywołanie funkcji fact_cps dostaje identyczność jako kontynuację 
początkową (gdy silnia jest obliczona, wystarczy ją zwrócić):

let fact n = fact_cps n (fun v -> v)

Dla drzew binarnych z zadania 2, napisz funkcję prod : int btree -> int, która liczy iloczyn 
wszystkich wartości w drzewie. Zapisz tę funkcję w CPS-ie, a następnie zmodyfikuj otrzymaną 
funkcję tak by w przypadku napotkania wartości 0 funkcja wykonała bezpośredni skok do miejsca 
swego wywołania, bez krokowego powracania z rekursji.
*)