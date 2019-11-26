# Statistische Lernverfahren
Ordner zum Schwender-Seminar

## What to do?
Wir hatten folgende Resultate erzielt:

#### Till:
- PCA-Verfahren angewendet: ohne Erfolg.
- Vorgehen gestaffelt nach intro/extro, anschließend für nächsten Faktor
- Interaktion versucht mit einzubeziehen, killte Prozessor
- Lemmatisierung mit Wortart ausgestattet: kein Erfolg
- Auftreten von Wörtern in Dokumenten statt absolut begrenzt

#### Matthias:
- Naive Bayes in R angewendet
- Entscheidungsbäume und Random Forests in R angewendet
- ein einzelner Baum liefert schlechtere Ergebnisse als ein Wald

#### Michael:
- Naive Bayes in R angewendet
- Random Forests in R angewendet (ca. 52 % Erfolg)
- Support Vector Machine in R angewendet (ca. 52 % Erfolg)
- Beim Stemming Infixe entfernen ( -zu-, bei -ge- nicht gut)

#### Tanja:
 - Count of Imperative, Fragen hinzugefügt: ohne Erfolg

#### Alex:
 - Random Forests in R angewendet

#### Jo:
 - Naive Bayes und Random Forests in Python angewendet
 - Vergleich Englisch/Deutsch, dazu deutsche Reviews von Holger mit DeepL teuer übersetzt
 - Beides mit SpaCy lemmatisiert
 - Auftreten von Wörtern in Dokumenten statt absolut begrenzt

Im Vortrag sollen folgende Themen von den einzelnen Personen angesprochen werden. Dazu sollte man sich die tex-Datei 
zum Grundvortrag anschauen und ein paar Folien zu seinem Thema erstellen.

<center>
  
| Name     | Theorie                                | Resultate                                                             |
|----------|----------------------------------------|-----------------------------------------------------------------------|
| Till     |  PCA und gestaffeltes Vorgehen         | zu selbigem                                                           |
| Matthias |  Entscheidungsbäume und Random Forest  | zu selbigem  (R/Deutsch Py/Englisch Py)                               |
| Michael  |  SVMen und Probleme                    | zu selbigem                                                           |
| Tanja    |  Aufstellen DTM und lineare Modelle    | zu linearen Modellen                                                  |
| Alex     |  Stemming und Lemmatisierung           | Wie sahen die Wortlisten, Grundformen aus? Wie viele? Aussagekräftig? |      
| Jonathan |  Naive Bayes                           | zu selbigem  (R/Deutsch Py/Englisch Py)                               |

</center>
