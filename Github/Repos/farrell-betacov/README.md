# farrell-betacov

Applying the Elmasri et al. combined model (scaled-phylogeny + affinity) approach to predict Betacoronavirus-host associations using the Bat-CoV data merged with HP3, with records are truncated to virual genus.

Source data are from ViromeNet/virionette (commit 33f56ad)

Four sets of predictions are made and are in the results folder:

- Bat-only associations x Elmasri full model: "FarrellBatFull.csv"

- Bat-only associations x Elmasri phylogeny-only model: "FarrellBatPhylogeny.csv"

- Mammal associations x Elmasri full model: "FarrellMammalsFull.csv"

- Mammal associations x Elmasri phylogeny-only model: "FarrellMammalsPhylogeny.csv"

The model predicts links internal to the documented association networks, and hosts which appear in the source phylogeny, but are not in the original data are added to the results file with NA as probability of interaction.


