TARGET_DIR=../../dev_rel/geonames
rm -rf $TARGET_DIR
rebar generate -f target_dir=$TARGET_DIR
