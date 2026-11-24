#### Problemi incontrati e soluzioni

Abbiamo iniziato rappresentando le variabili come semplici stringhe. Successivamente ci siamo trovati a dover gestire il renaming, la prima idea era la seguente: le variabili diventano una coppia stringa-intero, all'interno dell'albero di ogni espressione la variabile mantiene la sua stringa e le viene aggiunta una label intera che indica a quale astrazione è bindata. Questo processo di labeling veniva effettuato dalla funzione `label` ogni qual volta andasse ridotta una Eapp ((Evar str e1)) e2. 

L'implementazione prevedeva una hash map che per ogni stringa salvava il livello corrente di profondità, questo valore veniva assegnato come label ad ogni variabile incontrata (in questo modo per altro le variabili libere erano identificabili dal fatto di avere tutte indice 0). Ci si sono presentate diverse criticità:
1. "\a.\a2.\b.a2 c" produceva "\b.c", perchè la mappa teneva




report
mergiare tutto
test
io capire parser
readme decente