## Changes and Additions

- add application `jsonx` as deps
- add `httpc_client` module
- add some common functions in `riak_cs_utils` module
- add some moudle for statistic the download times,such as `riak_cs_s3_stats` `lib_riak`
- modify the flow for `riak_cs_s3_token` module when download a file
  
## Extra Configure

- configure the riak for search capability
- configure the schema for `priv/schema/dsd_cf_downloadtimes_schema.xml`
- configure CloudFile Server's address in `rel/files/advanced.config` 
  just like this:`{cf_host, "https://192.168.1.50:8443"}`