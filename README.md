# [Yurie/Cluster](https://github.com/yuriever/Yurie-Cluster)

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Wolfram Language](https://img.shields.io/badge/Wolfram%20Language-14.3%2B-red.svg)](https://www.wolfram.com/language/)

A Mathematica paclet for implementing a specific form of class composition.


## [Documentation](https://yuriever.github.io/symbolic/Yurie-Cluster/doc/)


## Usage

1. Clone or download this repository

2. Move the entire folder to the user paclet directory:

   ```wl
   $UserBasePacletsDirectory
   ```

3. Rebuild the paclet data:

   ```wl
   PacletDataRebuild[]
   ```

4. Load the paclet

    ```wl
    Needs["Yurie`Cluster`"]
    ```


### Uninstallation

```wl
PacletUninstall["Yurie/Cluster"]
```


### Installation checking

```wl
PacletFind["Yurie/Cluster"]
```
