(*
Zadanie 2 (8p.)

Mamy ciąg szklanek o znanych pojemnościach, a także kran i zlew. W każdym ruchu możemy 
wykonać jedną z trzech czynności:

	napełnić jedną ze szklanek wodą z kranu (FILL),
	opróżnić jedną ze szklanek do zlewu (DRAIN),
	przelać wodę z jednej ze szklanek do innej (TRANSFER).
	
Stan takiego układu szklanek możemy zapisać przy użyciu dwóch list reprezentujących 
odpowiednio pojemności szklanek i ilość wody znajdującą się w każdej z nich. Przykładowo, 
para ([4; 9], [4; 6]) oznacza że dysponujemy dwiema szklankami o pojemnościach 4 i 9, oraz że 
pierwsza z nich jest pełna, zaś w drugiej znajduje się 6 jednostek wody. W takim przypadku 
efekty poniższych ruchów następująco zmieniają tę zawartość szklanek (czyli drugą z 
powyższych list; zwróć uwagę że można napełnić pełną szklankę):

	FILL 1 → [4; 9]
	FILL 0 → [4; 6]
	DRAIN 0 → [0; 6]
	TRANSFER (0, 1) → [1; 9].
	
Dla danego zestawu szklanek i danej objętości wody, rozwiązaniem nazywamy ciąg ruchów, który 
prowadzi do uzyskania w dowolnej ze szklanek zadanej objętości wody. Przykładowo, dla szklanek 
[4, 9] i objętości 5, rozwiązaniem jest ciąg [FILL 1, TRANSFER (1, 0)] (a także wiele innych, 
redundantnych ciągów).

Użyj leniwych list aby zdefiniować funkcję nsols : (int list * int) -> int -> move list list, 
taką że nsols (glasses, volume) n zwróci listę n najkrótszych rozwiązań problemu zadanego 
przez glasses i volume (typ danych move powinien reprezentować pojedynczy ruch). W przypadku gdy 
rozwiązanie nie istnieje, program może się zapętlić.

Wskazówka: 
Ponieważ dopuszczamy redundantne ciągi (np. przelewanie z pustego w próżne), a 
także możemy się zapętlić gdy rozwiązanie nie istnieje, nie potrzeba sprawdzać czy daną 
konfigurację można osiągnąć w inny (potencjalnie lepszy) sposób.

Uwaga: 
Dzięki użyciu leniwych list, możliwa jest implementacja, w której w programie let f = 
nsols (g, v) in (f n; f n) drugie wywołanie f n działa w czasie O(n).
*)