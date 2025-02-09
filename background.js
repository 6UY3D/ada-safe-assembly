// Enhanced background script with better organization and error handling
import { StorageManager } from './utils/storage.js';
import { TweetScheduler } from './utils/scheduler.js';
import { ShadowbanChecker } from './utils/shadowban.js';
import { BookmarkManager } from './utils/bookmarks.js';

class BackgroundService {
  constructor() {
    this.storage = new StorageManager();
    this.scheduler = new TweetScheduler();
    this.shadowbanChecker = new ShadowbanChecker();
    this.bookmarkManager = new BookmarkManager();
    this.initializeListeners();
  }

  initializeListeners() {
    chrome.runtime.onMessage.addListener(this.handleMessage.bind(this));
    chrome.alarms.onAlarm.addListener(this.handleAlarm.bind(this));
  }

  async handleMessage(request, sender, sendResponse) {
    try {
      switch (request.type) {
        case 'SCHEDULE_TWEET':
          await this.scheduler.scheduleTweet(request.data);
          break;
        case 'CHECK_SHADOWBAN':
          await this.shadowbanChecker.check(request.username);
          break;
        case 'SAVE_BOOKMARK':
          await this.bookmarkManager.saveBookmark(request.tweet);
          break;
        default:
          console.warn(`Unknown message type: ${request.type}`);
      }
      sendResponse({ success: true });
    } catch (error) {
      console.error('Error handling message:', error);
      sendResponse({ success: false, error: error.message });
    }
    return true;
  }

  async handleAlarm(alarm) {
    if (alarm.name.startsWith('tweet-')) {
      await this.scheduler.processTweetAlarm(alarm);
    }
  }
}

// Initialize background service
const backgroundService = new BackgroundService();