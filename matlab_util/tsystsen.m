function sen = tsystsen(vga, pga, eq, f)

sen = -151+vga+pga+10*log10((f/106).^2./(1+(f/106).^2))+...
      10*log10((f/eq).^2./(1+(f/eq).^2));
