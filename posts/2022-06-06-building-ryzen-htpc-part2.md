---
title: Building a Low-power Ryzen HTPC, Part II
htmltitle: Building a Low-power Ryzen HTPC, Part II
---

_(this second part of Building a Low-power Ryzen HTPC series focuses on
underclocking, the first part is
[here](/posts/2022-01-15-building-ryzen-htpc-part1))_

I finally managed to get wattage readings from my Ryzen HTPC, and spend some
time figuring out the best way to underclock it. I used [PeakTech
9035](https://peaktech-rce.com/en/energy-meters/309-peaktech-9035-power-meter.html)
to measure power draw. This power meter also keeps note of the highest observed
power value, which comes in handy when doing stress tests.


My objectives are:

1. **Minimize power draw when idling**, which is what the HTPC will be doing
   most of the time. It makes the most sense to optimize for this first.

2. **Maximize performance when needed.** I will be using the HTPC as my
   development machine. When I'm doing a lot of compiling, I am more than
   willing to trade off power efficiency for performance and save some human
   time instead.

I also need to make sure I stay within the following constraints:

* Max CPU core temp must be **lower than 90° C** (194° F) at all times. The case is
  very space-constrained, with the only fan being the low-profile fan on the
  CPU. With limited air flow within the case, cooling performance of the fan is
  questionable. I want to make sure that I'm not frying the CPU during
  sustained load.

* Max power draw must not go over **80% of PSU declared power**. [The
  PSU](https://www.chieftec.eu/_getfs.php?tb=product_download&id=277&fs=fs1_en)
  I got has max declared power at 85W. I don't know how much power
  different components draw, so I want to leave some headroom for intermittent
  power spikes. With declared efficiency rating of 87%, the highest power draw
  I should be able to read at the mains is `(0.8 * 85W) / 0.87 = cca. 78W`.

For load testing, I used a 30-minute long `stress-ng` load test that combined
CPU hogs with I/O workers:

```bashext
stress-ng --cpu 6 --io 4 --hdd 2 --vm 2 --vm-bytes 128M --fork 4 --timeout 1800
```

I will take note of maximum CPU core temperature reached, maximum CPU frequency
observed, maximum power draw at the mains, and maximum PPT (AMD Package Power
Tracking) value, which roughly translates to how much power the CPU is
drawing[^1].

Before I list the values, I have to give a shoutout to [this great
article](https://sff.life/how-to-undervolt-ryzen-cpu/) on Ryzen CPU
undervolting. I took the advice on PPT tweaking and it worked great.

# Baseline: No underclocking

```txt
no underclocking
================
idle power draw (mains/PPT): 15.2 W /  4.4 W
max power draw  (mains/PPT): 68.7 W / 36.8 W
max CPU temp:                95 C
max CPU freq:                3952 Mhz
```

I was pretty impressed that idle power draw with no underclocking at all is as
good as idle power draw when CPU was severely underclocked. AMD power
management is pretty amazing. Additionally, PPT max value is very close to
declared TDP of 35W for my Ryzen CPU.

However, CPU temperature is unacceptably high during stress testing. Max power
draw was also uncomfortably close to my acceptable limit.


# Take one: Undervolting and frequency limiting

```txt
2000 Mhz max freq, 0.95 VID
===========================
idle power draw (mains/PPT): 15.5 W /  4.2 W
max power draw  (mains/PPT): 33.3 W / 13.8 W
max CPU temp:                60 C
max CPU freq:                2000 Mhz
```

This was my setting from the time I first set the HTPC up. It is very
conservative, and I knew up front that this configuration prioritizes efficiency
over performance. Both power draw and CPU temperature fall well within
acceptable limits, but performance is very hindered. It still works for Kodi
and editing, but falls short when it comes to compiling.

# Take two: PPT 20 W

```txt
PPT 20W
=======
idle power draw (mains/PPT): 16.4 W /  4.2 W
max power draw  (mains/PPT): 41.4 W / 20.0 W
max CPU temp:                76 C
max CPU freq:                3283 Mhz
```

The next thing I tried is to adjust Package Power Tracking (PPT), as suggested
in the article I mentioned above, and it worked out pretty well! Both power
draw and CPU temperature are fine, yet I was able to see CPU peaking at ~3.3
Ghz which made a big difference in performance.

At this point, I became a bit greedy, and wanted to see if I could squeeze a
bit more out of this.

# Final take: PPT 25W

```txt
PPT 25W
=======
idle power draw (mains/PPT): 16.9 W /  4.6 W
max power draw  (mains/PPT): 48.0 W / 25.0 W
max CPU temp:                87 C
max CPU freq:                3604 Mhz
```

This is my final underclocking configuration. To make sure CPU temperature
during load is stable, I ran the load test again for 1 hour, and the
temperature kept steady. Besides this, power draw during load testing remained
pretty efficient as well. Adjusting PPT for +5W resulted in about ~10% time
savings when doing intensive cpp compilation, when compared to PPT = 20W.


# Closing remarks

* This tiny PC is remarkably efficient. Even if it was running at full load
  during the entire month, it would contribute ~1 EUR to my electricity bill.

* Ryzen CPUs have great power management. While idling, cores were spending
  over 98% time in C6 sleep state. Power draw when idling was pretty much the
  same across all underclocking configurations, including baseline.

* Adjusting PPT is an amazing way to rein in CPU power consumption and get the
  most performance out of it.

[^1]: I was able to read package power thanks to this awesome [Ryzen SMU kernel
  driver](https://gitlab.com/leogx9r/ryzen_smu) provided by `leogx9r`.
