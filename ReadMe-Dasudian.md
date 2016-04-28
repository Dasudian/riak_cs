## Changes and Additions

- add application `jsonx` as deps
- add `httpc_client` module
- add some common functions in `riak_cs_utils` module 
- modify `riak_cs_s3_token` module for download a file:add statistic for download times,add judge 
  for file's state when download. 
  
## Extra Configure

- configure the riak for search capability
- configure the schema for `priv/schema/dsd_cf_downloadtimes_schema.xml`
- configure CloudFile Server's address in `rel/files/advanced.config` 
  just like this:`{cf_host, "https://192.168.1.50:8443"}`