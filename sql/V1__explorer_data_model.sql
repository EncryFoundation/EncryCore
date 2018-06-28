CREATE TABLE blocks (
  id VARCHAR(64) NOT NULL PRIMARY KEY,
  parent_id VARCHAR(64) NOT NULL,
  version SMALLINT NOT NULL,
  height INTEGER NOT NULL,
  ad_proofs_root VARCHAR(64) NOT NULL,
  state_root VARCHAR(66) NOT NULL,
  transactions_root VARCHAR(64) NOT NULL,
  ts BIGINT NOT NULL,
  difficulty BIGINT NOT NULL,
  block_size BIGINT NOT NULL,
  equihash_solution INTEGER ARRAY NOT NULL,
  ad_proofs BYTEA,
  tx_qty BIGINT NOT NULL DEFAULT 0,
  miner_address VARCHAR NOT NULL,
  miner_reward BIGINT NOT NULL,
  fees_total BIGINT NOT NULL,
  txs_size BIGINT NOT NULL
);

ALTER TABLE blocks OWNER TO encry_admin;

CREATE INDEX "blocks__parent_id" ON blocks (parent_id);

CREATE INDEX "blocks__height" ON blocks (height);

CREATE INDEX "blocks__ts" ON blocks (ts);

CREATE TABLE transactions (
  id VARCHAR(64) NOT NULL PRIMARY KEY,
  block_id VARCHAR(64) NOT NULL REFERENCES blocks (id),
  is_coinbase BOOLEAN NOT NULL,
  ts BIGINT NOT NULL
);

ALTER TABLE transactions OWNER to encry_admin;

CREATE INDEX "transactions__block_id" on transactions (block_id);

CREATE TABLE outputs (
  id VARCHAR(64) NOT NULL PRIMARY KEY,
  tx_id VARCHAR(64) NOT NULL REFERENCES transactions (id),
  monetary_value BIGINT NOT NULL,
  coin_id VARCHAR NOT NULL,
  contract_hash VARCHAR NOT NULL,
  data BYTEA
);

ALTER TABLE outputs OWNER to encry_admin;

CREATE INDEX "outputs__tx_id" on outputs (tx_id);

CREATE INDEX "outputs__contract_hash" on outputs (contract_hash);

CREATE TABLE inputs (
  id VARCHAR(64) NOT NULL PRIMARY KEY,
  tx_id VARCHAR(64) NOT NULL REFERENCES transactions (id),
  serialized_proofs VARCHAR NOT NULL
);

ALTER TABLE inputs OWNER to encry_admin;

CREATE INDEX "inputs__tx_id" on inputs (tx_id);