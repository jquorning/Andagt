Brug: andagt port andagtfile
      port - port nummer. Fx 1000
      andagtfile - data med andagter i formatet
                 07-18 http://link.to.server/page.html -- Angagt for 07-18 (18. Juli)
                 
Starter et program som henviser til dagens andagt ved besøg på siden localhost:port.

Eksempler:

localhost:1000   -- andagt of today
localhost:1000/day=today  -- the same
localhost:1000/day=2020-07-18 -- andagt of 2020-07-18
localhost:1000/day=18-07-2020 -- andagt of 2020-07-18
localhost:1000/day=tomorrow
localhost:1000/day=yesterday
localhost:1000/day=imorgen
localhost:1000/day=igår
localhost:1000/day=idag
localhost:1000/toc  -- indholdsfortegnelse

Port nummer kan undgås ved at vælge oprt 80.
Der benyttes altid http som protokol (ikke krypertet)
