#lang notes

@; scribble --pdf crypto_mining.scrbl   # compile
@; evince crypto_mining.pdf & disown    # view / verify

@block{
  @block-name{Crypto Mining - General}
  Coin type - @url{https://en.wikipedia.org/wiki/Cryptocurrency#Altcoins}

  Electricity price

  Janez

  BTC Mining @url{https://en.wikipedia.org/wiki/Bitcoin_network#Mining}
  @url{https://en.bitcoin.it/wiki/Pooled_mining}

  @hyperlink["https://en.bitcoin.it/wiki/Main_Page"]{Bitcoin Wiki}

  Cloud mining - in a remote datacenter

  algorithms
  block reward
}

@block{
  @block-name{Mining}
  @block{
    @block-name{General}
    CPU vs GPU
    @url{https://blockmine.de/en}
    Rigs
    @url{https://www.ebay.de/b/Mining-Rigs/179171/bn_81941746}
    Size
    Cooling
    Noise
    Power consumption

    @url{https://blockmine.de/en/collections/hardware-mit-hosting}
    Hosted Hardware

    riser board - gives a computer motherboard the option for additional expansion
    cards
  }

  @block{
    @block-name{Chia mining}
    @hyperlink["https://youtu.be/QfaIGum5fCE"]{YT video: Chia mining}

    XCH Chia "green" crypto currency - requiring less power for mining
    Chia coin - 30$ as of 2022-04-06
    uses Proof-of-Capacity i.e. Proof-of-Space. Allocate lots of memory or disk space. Similar to PoW. Instead of computation storage is used

    Chialisp blockchain smart transaction programming language

    Helium miner
  }

  @block{
    @block-name{GPU Mining Rig Buying Guide}
    @hyperlink["https://youtu.be/hNqs3Fn8TNo"]{YT Video: 2021-01-13: GPU Mining Rig Buying Guide - All You Need To Know | The Basics}
    GPU miner - a PC with typically multiple GPUs

    GPUs and PSUs are the main components of a mining rig

    mining specific motherboards

    Graphic cards
    AMD Radeon 6900 XT

    @url{https://whattomine.com/}
    JSON API @url{https://whattomine.com/coins.json}
    Estimate how much a given Video Card will be making

    cryptex.com
    @url{https://mineshop.eu}

    scams on alibaba or alixpress

    bots: e.g. stockradar / stockraider - stock monitoring server on @url{https://discord.gg}
  }

  @block{
    @block-name{How To Build A GPU Mining Rig | The Basics}
    @hyperlink["https://www.youtube.com/c/MiningChamber"]{YT channel: MiningChamber}
    @hyperlink["https://youtu.be/kRKNcyoCSeA"]{YT video: 2021-03-21: How To Build A GPU Mining Rig | The Basics}
  }

  @block{
    @block-name{Mining pools - MPs}
    take at least 1% of the profit
    @url{https://en.wikipedia.org/wiki/Mining_pool}
    @hyperlink["https://youtu.be/0iPrbK1thSw"]{YT video: 2021-02-16: Mining Pools Explained - Dashboard, Payment Structures & More | Ft. Flexpool}

    MP Payout Fee Policies
  }

  @block{
    @block-name{Examples}
    @url{https://flexpool.io}
    @url{https://ethermine.org/}
    @url{https://miningpoolstats.stream} - prefer "near" pools due to the latency
  }

  @block{
    @block-name{Reward systems}
    both yield about the same profit
    PPLNS - Pay Per Last n Shares - paid what you mine on the pool
    pros: payouts coming from the block rewards
    cons: infrequent rewards

    PPS - Pay Per Share (a bit like a bank, a bit shady)
    pros: stable income, no matter if the a block was found
    cons: payouts coming from pool's wallet

    FPPS

    PPS+
  }

  @block{
    @block-name{Shares}
    effective
    reported
    valid
    stale - delivered to the pool after the block was mined
    invalid - incorrect solutions

    share difficulty vs mining difficulty
    - set by the pool
  }
}

@block{
  @block-name{Parts}
  @block{
    @block-name{Essentials}
    Motherboards CPU RAM Storage Risers Network Frame
  }

  @block{
    @block-name{Main parts}
    GPUs PSU(s)

    compatibilities
    which GPUs are the best
    Static electricity - avoid carpets, ground yourself
    don't touch the gold tips on memory modules

    AAA wave frame: aaawave.com
  }

  @block{
    @block-name{PSU - Power Supply}
  }

  @block{
    @block-name{Server power supplies}
    generally better; don't use them when mining in the living or bed room
  }

  @block{
    @block-name{ATX power supplies}
    mostly used; most of the time you will run short on power; good when mining in the living or bed room

    because you want to avoid using SATA for the risers, because they can't supply enough power
    SATA will most likely burn out sooner or later

    Powering risers - through the 6-pins on the riser itself

    Splitters - basically give more pins

    draft out everything

    fire extinguisher for electronics

    server power supply - @url{https://parallelminer.com}
    how many GPUs - see number of PCIE slots on the motherboard, power of power supply
    how to wire
    pin calculations

    dust buildup

    GPUs and risers - properly distribute the power
    molex cable
    motherboard, breakout board
    mininghub - site

    See buying strategy guide -
  }
}

@block{
  @block-name{ANTMINER S19XP 140TH}
  @url{https://blockmine.de/en/products/antminer-s19-xp}
  ANTMINER S19XP from Bitmain with a max hash rate of 140 TH/s was used for mining
  the SHA-256 algorithm with a power consumption of 3010 W±5% and an energy
  efficiency of 21.5 W/Th developed. The mining servers are easy to deploy and
  adapt to mining farms of different sizes.

  Technical specifications:
  Condition: new
  Version: S19XP
  Algorithm: SHA-256
  Power consumption: 3010 watts
  Energy Efficiency: 21.5 Watts / TH
  Dimensions: 400x195x295mm
  Weight: 14.4 kg
}

@block{
  @block-name{Blockmine Data Gmbh}
  Magirus-Deutz-Straße 12, 89077 Ulm
  info@"@"blockmine.de
  +49 731 14113790
}

@block{
  @block-name{News - from palmabot}
  @url{https://news.bitcoin.com/}

  @url{https://blockchain.news}
  @url{https://blockchain.news/tag/mining}

  @url{https://www.coindesk.com/}
  @url{https://bitcoinmagazine.com/}
  @url{https://bitcoinist.com/}
  @url{https://cryptoslate.com/}
  @url{https://www.newsbtc.com/}
  @url{https://www.investing.com/}
  @url{https://www.reddit.com/r/CryptoCurrency/}
  @url{https://cointelegraph.com/}
  @url{https://coingape.com/}
  @url{https://medium.com/@"@"thecapital}
  @url{https://cryptobriefing.com/}
  @url{https://www.cryptoninjas.net/}
  @url{https://coinpedia.org/}
}
