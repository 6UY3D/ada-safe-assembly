// Tweet scheduling utility
export class TweetScheduler {
  constructor() {
    this.storage = new StorageManager();
  }

  async scheduleTweet(tweetData) {
    const scheduledTweets = await this.storage.get('scheduledTweets') || [];
    
    const newTweet = {
      id: crypto.randomUUID(),
      content: tweetData.content,
      scheduledTime: tweetData.scheduledTime,
      thread: tweetData.thread || false,
      media: tweetData.media || [],
      status: 'pending'
    };

    scheduledTweets.push(newTweet);
    await this.storage.set('scheduledTweets', scheduledTweets);
    
    chrome.alarms.create(`tweet-${newTweet.id}`, {
      when: new Date(tweetData.scheduledTime).getTime()
    });

    return newTweet.id;
  }

  async processTweetAlarm(alarm) {
    const tweetId = alarm.name.replace('tweet-', '');
    const scheduledTweets = await this.storage.get('scheduledTweets') || [];
    
    const tweetIndex = scheduledTweets.findIndex(t => t.id === tweetId);
    if (tweetIndex === -1) return;

    const tweet = scheduledTweets[tweetIndex];
    try {
      await this.postTweet(tweet);
      scheduledTweets[tweetIndex].status = 'posted';
    } catch (error) {
      scheduledTweets[tweetIndex].status = 'failed';
      scheduledTweets[tweetIndex].error = error.message;
    }

    await this.storage.set('scheduledTweets', scheduledTweets);
  }

  async postTweet(tweet) {
    // Implementation for posting tweet through XPro's web interface
    // This would involve DOM manipulation to trigger the tweet posting
  }
}