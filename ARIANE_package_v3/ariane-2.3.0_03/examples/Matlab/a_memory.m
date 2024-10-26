%% READ and plot Ariane meory log file %%

[memory, mem_r, mem_i, mem, tabname, subname] = ...
  textread('ariane_memory.log','%d %d %d %d %s %s',-1);
plot((memory/1024)/1024);
hold on
plot ((mem_r/1024)/1024,'r');
plot ((mem_i/1024)/1024,'g');
title('Ariane memory log in MegaBytes (MB)');
ylabel('Memory in MB');
xlabel('Number of allocate and deallocate');
legend('total','real','integer');
