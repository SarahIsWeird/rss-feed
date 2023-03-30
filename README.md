# rss-feed

rss-feed is going to be a small RSS reader written in Haskell. Don't expect good Haskell code, this is my first project on my own :D

## Usage

### Add a feed

```
rss-feed add <feedName> <rssUrl>
```

### Remove a feed

```
rss-feed remove <feedName>
```

### List feeds

```
rss-feed feeds
```

### List posts from a feed

```
rss-feed posts <feedName>
```

### Mark a post as read

```
rss-feed mark-read <feedName> <postTitle>
```

