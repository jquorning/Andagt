# Andagt
Starter et program som henviser til dagens andagt ved besøg på siden localhost:port.

## Requires
* Ada compiler
* GPRbuild

## Build
```sh
$ gprbuild
```

## Run

```sh
$ ./andagt andagt.file port
```

andagt.file - data med andagter i formatet
port - port nummer. Fx 1000 (Default 80)
Port nummer kan undgås ved at vælge oprt 80.
Der benyttes altid http som protokol (ikke krypertet)
                 
## Format of andagt.file
Hver linje indeholder en dato samt link tilhørende dato. Fx:

```sh
07-18 http://link.to.server/page.html  -- Angagt for 07-18 (18. Juli)
07-19 http://link.to.serter/page2.html -- Andagt for 07-19 (19. Juli)
```

`--` eller `#` angiver en kommentar, der ignoreres
Tomme linier ignoreres også

# URI format
Eksempler:

URI                           | Beskrivelse    
----------------------------- | -------------------
localhost:1000/toc            | Indholdsfortegnelse
localhost:1000                | Dagens andagt
localhost:1000/day=today      | Dagens andagt
localhost:1000/day=idag       | Dagens andagt
localhost:1000/day=2020-07-18 | Andagt for 2020-07-18
localhost:1000/day=18-07-2020 | Andagt for 2020-07-18
localhost:1000/day=tomorrow   | Morgendagens andagt
localhost:1000/day=imorgen    | Morgendages andagt
localhost:1000/day=yesterday  | Gårsdagens andagt
localhost:1000/day=igår       | Gårsdagens andagt

