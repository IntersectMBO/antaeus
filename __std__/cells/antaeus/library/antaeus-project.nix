{ inputs, cell }:

cell.library.make-antaeus-project {
  deferPluginErrors = false;
  enableHaskellProfiling = false;
}
