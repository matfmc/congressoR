# congressoR
Pacote R de funções para as API'S do congresso.

Este pacotes traz funções que facilitam a conectividade entre o R e as Api's de dados abertos do Congresso Federal. No momento apenas alguns endpoints da Câmara dos Deputados.


`https://dadosabertos.camara.leg.br/swagger/api.html`

## Exemplos :

```R
df <- GETDeputados(idLegislatura = '56')
head(df[["dados"]][,c(3,4,6,7,9)])

              nome siglaPartido siglaUf idLegislatura                            email
1   Abílio Santana           PL      BA            56  dep.abiliosantana@camara.leg.br
2        Abou Anni          PSL      SP            56       dep.abouanni@camara.leg.br
3   Acácio Favacho         PROS      AP            56  dep.acaciofavacho@camara.leg.br
4     Adolfo Viana         PSDB      BA            56    dep.adolfoviana@camara.leg.br
5  Adriana Ventura         NOVO      SP            56 dep.adrianaventura@camara.leg.br
6 Adriano do Baldy           PP      GO            56 dep.adrianodobaldy@camara.leg.br
```


