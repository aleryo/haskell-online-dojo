# haskell-sqlite

## Plan

1. Interpréteur de commandes
2. Analyseur syntaxique SQL
3. Transformation SQL -> Algèbre relationnelle simple
4. Interprétation `SELECT ... FROM ...` en mémoire
5. Écriture sur disque
6. Les curseurs
7. Structures de B-Arbres
8. Interprétation des opérateurs relationnels sur les B-arbres
9. Optimisation du plan de requête
10. Serveur REST

### Pipeline

```
parseSQL :: Text -> SQL
```

Analyse syntaxique d'un texte pour produire une expression SQL

```
 toRelational :: SQL -> Relational
```

Transforme une expression SQL vers algèbre relationnelle

```
evaluateDB :: Relational -> DB -> Either Error (Relation, DB)
```

Interprète une expression relationnelle dans le contexte d'une BDD (caché derrière `Database` qui est une monade)

### Logs

```
stream :: Relational -> DB -> [ DBOps ]
```


## Sessions

* [Session 3](https://www.youtube.com/watch?v=EAi--VC_DhY)
* [Session 4](https://www.youtube.com/watch?v=tDP_OjiBO_w)
* [Session 5](https://www.youtube.com/watch?v=eNYeYcgUvAo)
* [Session 6](https://www.youtube.com/watch?v=u-UdEeig-0U)
* [Session 7](https://youtu.be/C3owSoky7M0)
* [Session 8](https://youtu.be/eGZB0VltlKY)
* [Session 9](https://www.youtube.com/watch?v=1RAZ_hjeJCU)
* [Session 10](https://www.youtube.com/watch?v=2IY-JNcfal8)
* [Session 11](https://www.youtube.com/watch?v=1Dh_dAmx7Iw)
* [Session 12](https://www.youtube.com/watch?v=65GfONxycXo)
