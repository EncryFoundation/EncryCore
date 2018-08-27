CREATE TABLE headers(
  id VARCHAR(64) PRIMARY KEY,
  parent_id VARCHAR(64) NOT NULL,
  version SMALLINT NOT NULL,
  height INTEGER NOT NULL,
  ad_proofs_root VARCHAR(64) NOT NULL,
  state_root VARCHAR(66) NOT NULL,
  transactions_root VARCHAR(64) NOT NULL,
  ts BIGINT NOT NULL,
  nonce BIGINT NOT NULL,
  difficulty BIGINT NOT NULL,
  block_size BIGINT NOT NULL,
  equihash_solution INTEGER ARRAY NOT NULL,
  ad_proofs VARCHAR DEFAULT '',
  tx_qty BIGINT NOT NULL DEFAULT 0,
  miner_address VARCHAR NOT NULL,
  miner_reward BIGINT NOT NULL,
  fees_total BIGINT NOT NULL,
  txs_size BIGINT NOT NULL,
  best_chain BOOLEAN NOT NULL
);

CREATE TABLE transactions(
  id VARCHAR(64) PRIMARY KEY,
  fee BIGINT NOT NULL,
  block_id VARCHAR(64) REFERENCES headers (id),
  is_coinbase BOOLEAN NOT NULL,
  ts BIGINT NOT NULL
);

CREATE TABLE outputs(
  id VARCHAR(64) PRIMARY KEY,
  tx_id VARCHAR(64) REFERENCES transactions (id),
  monetary_value BIGINT NOT NULL,
  coin_id VARCHAR NOT NULL,
  contract_hash VARCHAR NOT NULL,
  data VARCHAR
);

CREATE TABLE inputs(
  id VARCHAR(64) PRIMARY KEY,
  tx_id VARCHAR(64) REFERENCES transactions (id),
  contract_bytes TEXT NOT NULL,
  serialized_proofs VARCHAR NOT NULL
);

CREATE TABLE directives(
  tx_id VARCHAR(64) REFERENCES transactions (id),
  type_id SMALLINT NOT NULL,
  is_valid BOOLEAN NOT NULL,
  contract_hash SMALLINT[] NOT NULL,
  amount BIGINT NOT NULL,
  address TEXT NOT NULL,
  token_id_opt SMALLINT[],
  data_field SMALLINT[] NOT NULL
);