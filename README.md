## Geonames Erlang Application / Node



### License
All code in this library comes under the P2P Production License 
http://p2pfoundation.net/Peer_Production_License

Essentially, you can use it for fun or to learn, or as part of an open source library / system for free, 
but a commerical enterprise should request the ability to use, change or update the code for their commercial purpose. 
This may incur a fee. Please contact us through github to discuss your requirements

### Rationale 

Geonames.org provides a large dataset of geographical information. 

This application exists in order to expose this data as a node backed by an mnesia database.

The reason to make this a standalone node is because the data is then accessible to the whole cluster, and the 'application' mnesia db is not filled with reference data


### Setup

Download the data files `cd scripts && bash get_files.sh`

### Build

#### Application

`cd apps/geonames && rebar compile`

#### Node 

*Note* You may need to edit the reltool.config file and remove `{excl_lib, otp_root},` and probably you will need to add nodetool from another rebar project.

`cd rel && bash gen_dev_rel.sh`

#### Client

`cd apps/geonames_client && rebar compile`

#### Deployment

After creating the release, you need to tell the node where the data is located

From the root folder, after generating the node: `cd dev_rel/geonames


### API

#### Direct API: 

See the module `geonames.erl` for exports

#### Client API: 

From another node you can call 

### Documentation

There isn't a lot due to time pressures. Most calls will return a proplist of various fields, or a list of binary strings

### Design

`geonames_mnesia.erl` is the DB layout. There is a more or less a 1-1 mapping of fields from the text files to the 


