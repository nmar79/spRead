# spRead
Estimate distances between specimen groups 

Description
Functions to calculate within group distances, distances from group centres to the origin, angles between group centres, and distances between group centroids.

Details
FUNCTIONS

within_group_distances(): bootstrapped median distance between specimens in each group

distance_from_origin(): bootstrapped distance of group centroids from origin

angle_between_centroids(): angle between group centroids

proc_dist_group(): Procrustes distances between group centroids

DATASETS

(1) sheep_lmrks_202212180740.TPS: landmarks and semisliding landmarks of Iron Age sheep astragals (Harding & Marom 2022) from three sites in northern Israel (Tel Dor, Tel Keisan, Tel Abel Beit Maacah). Available at https://doi.org/10.1101/2022.12.24.521859

(2) sheep_sliders_202212180739.csv: slider configuration file for the sheep landmark data in (1). Available at https://doi.org/10.1101/2022.12.24.521859

(3) sheep_specinfo_20230129.csv: information on the astragali in (1). Available at https://doi.org/10.1101/2022.12.24.521859 [NOTE that the column title of the file in this version is 'Context', and should be changed to 'group']

(4) spec_info.Rdata: data frame with specimens in rows, site (group) assignment in the first column, and PCA scores in consecutive columns. The data were transformed and ordinated using the gpagen() and gm.prcomp() functions from the library geomorph (Adams et al. 2022, Baken et al. 2021).

REFERENCES

Adams, D. C., M. L. Collyer, A. Kaliontzopoulou, and E.K. Baken. 2022. Geomorph: Software for geometric morphometric analyses. R package version 4.0.4. https://cran.r-project.org/package=geomorph.

Baken, E. K., M. L. Collyer, A. Kaliontzopoulou, and D. C. Adams. 2021. geomorph v4.0 and gmShiny: enhanced analytics and a new graphical interface for a comprehensive morphometric experience. Methods in Ecology and Evolution. Methods in Ecology and Evolution. 12:2355-2363.

Harding, S. A., & Marom, N. (2022). Shape analysis of Iron Age sheep astragali suggests west-to-east morphotype diffusion in the southern Levant. bioRxiv, 2022-12. doi: https://doi.org/10.1101/2022.12.24.521859

AUTHOR

Nimrod Marom, nimrod.arch@gmail.com | https://sites.google.com/view/nimrodmarom/home
