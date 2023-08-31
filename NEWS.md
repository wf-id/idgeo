<!--- Newest updates go at the top --->

<!--- Always increment the Version when pushing changes to the main branch  --->
<!--- Use a level 1 section head "Changes in version X.X.X" each update     --->
<!--- For many changes, use level 2 sections headers:                       --->
<!---         e.g. breaking changes, new features, bug fixes, etc           --->

# idgeo (development version 0.0.0.9000)

* Initial creation

## Datasets

data frames:

1. `local_zips`
2. `nc_counties`

sf:

3. `sf_nc_counties`
4. `sf_nc_tracts`
5. `sf_nc_zips`

## Generic Functions

1. `generate_average_distance_centroid()`
2. `put_pts_in_polygon()`

## Leaflet Functions

Lots! The main ones: `prettyLeaflet()` in place of `leaflet::leaflet()` and `addPrettyPolygons()` in place of `leaflet::addPolygons()` for consistent and efficient leaflet map making.
