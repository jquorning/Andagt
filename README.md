# Andagt
Starter et program som henviser til dagens andagt ved besøg på siden localhost:8080.
Der benyttes altid http som protokol (ikke krypertet)

## Requires
* Ada compiler
* GPRbuild

## Build
```sh
$ gprbuild
```

## Run

```sh
$ ./andagt andagt.file [port]
```

Parameter.  | Betydning
----------- | ------------
andagt.file | Data file med andagter
      
## Format of andagt.file
Hver linje indeholder en dato samt link tilhørende dato. Fx:

```sh
07-18 http://link.to.server/page.html  -- Andagt for 07-18 (18. Juli)
07-19 http://link.to.serter/page2.html -- Andagt for 07-19 (19. Juli)
```

`--` eller `#` angiver en kommentar, der ignoreres
Tomme linier ignoreres også

## URI format
Eksempler:

URI                           | Beskrivelse    
----------------------------- | -------------------
localhost:8080/toc            | Indholdsfortegnelse
localhost:8080                | Dagens andagt
localhost:8080/day=today      | Dagens andagt
localhost:8080/day=idag       | Dagens andagt
localhost:8080/day=2020-07-18 | Andagt for 2020-07-18
localhost:8080/day=18-07-2020 | Andagt for 2020-07-18
localhost:8080/day=tomorrow   | Morgendagens andagt
localhost:8080/day=imorgen    | Morgendages andagt
localhost:8080/day=yesterday  | Gårsdagens andagt
localhost:8080/day=igår       | Gårsdagens andagt
