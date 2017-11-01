(*
Zadanie 5 (6p.)

Przy modyfikacji struktur danych często chcemy wykonać więcej niż jedną operację w pobliskich 
miejscach. W przypadku czysto funkcyjnych struktur może to skutkować niepotrzebnie długim czasem 
wykonania, spowodowanym przechodzeniem i odtwarzaniem struktury danych przy każdej zmianie. Aby 
zapobiec temu problemowi, możemy rozbić operację modyfikacji struktury na dwie części: 
znajdowania miejsca w strukturze, w którym mamy wykonać zmianę, oraz przeprowadzenia zmiany w 
konkretnym miejscu; musimy też znaleźć odpowiednią pośrednią reprezentację dla takich miejsc.

	1. Zdefiniuj typ danych α place jako pośrednią reprezentację dla typu α list.
	
	2. Zaimplementuj funkcje findNth : α list -> int -> α place oraz collapse : α place -> 
α list. Pierwsza powinna lokalizować n-te miejsce na liście, druga — zapominać informację o 
miejscu i zwracać listę na której działamy.

	3. Zaimplementuj funkcje add : α -> α place -> α place oraz del : α place -> α place, 
odpowiednio dodającą i usuwającą element w odpowiednim miejscu listy. Funkcje powinny działać 
w czasie stałym, oraz spełniać następujące równości: 
		(a) collapse (add 3 (findNth [1;2;4] 2)) = [1;2;3;4]
		(b) collapse (del (findNth [1;2;4] 2)) = [1;2]
		(c) del (add x p) = p dla dowolnych x : α i p : α place
		
	4. Zaimplementuj funkcje next : α place -> α place oraz prev : α place -> α place, 
służące do nawigacji w strukturze listy.

	Listy nie są jedynym typem danych dla którego możemy definiować struktury pamiętające 
miejsce w którym pracujemy. Zdefiniuj typ danych α btplace będący pośrednią reprezentacją 
pozwalającą na modyfikacje drzew binarnych z Zadania 2. Napisz 3 funkcje up, left, right : α 
place -> α place do nawigacji po drzewie.
*)