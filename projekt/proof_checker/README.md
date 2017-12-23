# Proof checker
#### Wojtek Fica

### Specyfikacja
Implementacja podstawowej wersji zadania. 
Bez dodatków. 
Obsługuje logikę klasyczną - dodane prawo eliminacji podwójnej negacji.

### Struktura projektu
Najważniejszy fragment zadania jest napisany w plikach
- [rules.ml](rules.ml)
- [natural_deduction.ml](natural_deduction.ml)

### Kompilacja
- kompilacja: ```make proof_checker```
- sprzątanie: ```make clean```

### Uruchomienie
```./proof_checker.native in out```

### Składnia
Składnia jak w specyfikacji zadania, ale:
- nazwa zmiennej dopasować się do następującego wyrażenia regularnego: ```['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*```
- nazwa celu dopasować się do następującego wyrażenia regularnego: ```['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*```

### Testy
Dostarczone testy:
- tests/in0 - test z poprawnymi dowodami
- tests/in1 - test z błędnymi dowodami

### Plan
- dodanie aksjomatów
- dodanie kwantyfikatorów
- może umożliwienie programowi dopowiedzienia sobie pominiętych fragmentów dowodów
